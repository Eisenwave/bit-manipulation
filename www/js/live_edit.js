import { container, editorFractionItem, editorFractionLimit, isEditorVertical, resizeContainerToFraction, setEditorVertical } from "./live_edit_core.js";

const whileDraggingClass = 'while-dragging';

const separator = document.getElementById('separator');
const splitVerticalButton = document.getElementById('button-split-vertical');
const splitHorizontalButton = document.getElementById('button-split-horizontal');
const codeInput = document.getElementById('code-input');
const inputLineNumbers = document.getElementById('input-line-numbers');

let isDragging = false;

Number.prototype.clamp = function (min, max) {
    return Math.min(Math.max(this, min), max);
};

String.prototype.count = function (x) {
    let n = 0;
    for (const c of this) {
        if (x === c) {
            ++n;
        }
    }
    return n;
};

async function loadWasm() {
    const imports = {};
    const result = await WebAssembly.instantiateStreaming(fetch('/bm.wasm'), imports);

    if (result.instance.exports._initialize) {
        result.instance.exports._initialize();
    } else {
        console.warn('module has no export _initialize');
    }

    return result;
}

/**
 * Determines the fraction (horizontal or vertical) of the cursor position.
 * @param {DOMRect} rect the rectangle
 * @param {MouseEvent} e the mouse event
 * @param {boolean} vertical if true, calculates the fraction based on y/height
 * @returns {number} a fraction between `0` and `1`
 */
function determineDragFraction(rect, e, vertical = false) {
    if (vertical) {
        const offsetY = e.clientY - rect.top;
        return offsetY / rect.height;
    } else {
        const offsetX = e.clientX - rect.left;
        return offsetX / rect.width;
    }
}

separator.addEventListener('mousedown', (e) => {
    isDragging = true;
    document.body.classList.add(whileDraggingClass);
});

document.addEventListener('mousemove', (e) => {
    if (!isDragging) {
        return;
    }

    const f = determineDragFraction(container.getBoundingClientRect(), e, isEditorVertical())
        .clamp(editorFractionLimit, 1 - editorFractionLimit);

    resizeContainerToFraction(f);
    localStorage.setItem(editorFractionItem, f);
});

document.addEventListener('mouseup', () => {
    if (isDragging) {
        isDragging = false;
        document.body.classList.remove(whileDraggingClass)
    }
});

const persistOrientationChanges = true;

splitVerticalButton.addEventListener('click', (e) => {
    e.preventDefault();
    setEditorVertical(true, persistOrientationChanges);
});

splitHorizontalButton.addEventListener('click', (e) => {
    e.preventDefault();
    setEditorVertical(false, persistOrientationChanges);
});

let currentLineNumbers = 1;

function setVisibleLineNumbers(n) {
    if (currentLineNumbers === n) {
        return;
    }
    const arr = Array.from({ length: n }, (_, i) => i + 1);
    inputLineNumbers.innerText = arr.join('\n');
    currentLineNumbers = n;
}

codeInput.addEventListener('input', () => {
    setVisibleLineNumbers(codeInput.value.count('\n') + 1);
});

codeInput.addEventListener('scroll', () => {
    inputLineNumbers.scrollTo({ top: codeInput.scrollTop });
});

const wasm = await loadWasm();

/**
 * Adds two integers.
 * @param {number} x 
 * @param {number} y 
 * @returns {number}
 */
function bmPlus(x, y) {
    return wasm.instance.exports.bm_plus(x, y);
}

/**
 * Allocates `n` bytes in the WASM instance.
 * @param {number} n the amount of bytes to allocate
 * @returns {{memory: number, size: number}} 
 */
function bmAlloc(n) {
    return { memory: wasm.instance.exports.bm_foreign_alloc(n), size: n };
}

/**
 * Frees memory in the WASM instance, previously allocated with `bmAlloc`
 * or obtained through another allocating function.
 * @param {{memory: number, size: number}} param0 the allocation, obtained from `bmAlloc` 
 */
function bmFree({ memory, size }) {
    wasm.instance.exports.bm_foreign_free(memory, size);
}

/**
 * Converts a `string` to a UTF-8 encoded array, allocated in the WASM instance.
 * @param {string} str the string
 * @returns {{memory: number, size: number}}
 */
function bmStringToUtf8(str) {
    const data = new TextEncoder().encode(str);
    const allocation = bmAlloc(data.byteLength);

    const wasmMemoryView = new Uint8Array(wasm.instance.exports.memory.buffer);
    wasmMemoryView.set(data, allocation.memory);

    return allocation;
}

/**
 * Decodes a `bm_allocation` object located at the given address.
 * @param {number} address the address in WASM memory of the object
 * @returns {{memory: number, size: number}}
 */
function bmDecodeAllocationAt(address) {
    const view = new DataView(wasm.instance.exports.memory.buffer);
    return {
        memory: view.getUint32(address, true),
        size: view.getUint32(address + 4, true)
    };
}

/**
 * Measures the length of the string and returns the result as an allocation.
 * @param {string} str 
 * @returns {{memory: number, size: number}}
 */
function bmLengthAsUtf8(str) {
    const input = bmStringToUtf8(str + '\0');
    wasm.instance.exports.bm_length_as_string(input.memory);
    bmFree(input);

    const resultAddress = wasm.instance.exports.bm_length_as_string_result.value;
    return bmDecodeAllocationAt(resultAddress);
}

/**
 * Converts allocated UTF-8 data in WASM to a `string`.
 * @param {{memory: number, size: number}} param0 
 * @returns {string}
 */
function bmUtf8ToString({ memory, size }) {
    const wasmMemoryView = new Uint8Array(wasm.instance.exports.memory.buffer);
    const utf8Bytes = wasmMemoryView.slice(memory, memory + size);
    const decoder = new TextDecoder("utf-8", { fatal: true });
    return decoder.decode(utf8Bytes);
}

codeInput.addEventListener('input', () => {
    const result = bmLengthAsUtf8(codeInput.value);
    console.log(codeInput.value.length, bmUtf8ToString(result));
    bmFree(result);
});

function debugStuff() {
    console.debug(bmPlus(1, 2));
    const p = bmAlloc(1000);
    console.debug(p);
    bmFree(p);

    const h = bmStringToUtf8('awoo');
    console.debug(h);
    bmFree(h);

    const lengthResult = bmLengthAsUtf8('awoo');
    console.debug(lengthResult);
    console.debug(bmUtf8ToString(lengthResult));
    bmFree(lengthResult);
}

// debugStuff();