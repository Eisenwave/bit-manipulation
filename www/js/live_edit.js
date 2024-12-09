import { container, editorFractionItem, editorFractionLimit, isEditorVertical, resizeContainerToFraction, setEditorVertical } from "./live_edit_core.js";

const whileDraggingClass = 'while-dragging';

const separator = document.getElementById('separator');
const splitVerticalButton = document.getElementById('button-split-vertical');
const splitHorizontalButton = document.getElementById('button-split-horizontal');


let isDragging = false;

Number.prototype.clamp = function (min, max) {
    return Math.min(Math.max(this, min), max);
};

const importObject = {
    env: {
        memory: new WebAssembly.Memory({ initial: 256 }),
        //table: new WebAssembly.Table({ initial: 10, element: 'anyfunc' }),
        __stack_pointer: new WebAssembly.Global({ value: 'i32', mutable: true, }, 1024 * 1024),
        __memory_base: 0,
        __table_base: 0
    }
};

WebAssembly.instantiateStreaming(fetch('/bm.wasm'), importObject)
    .then(result => {
        console.log(result);
        if (result.instance.exports.__wasm_call_ctors) {
            result.instance.exports.__wasm_call_ctors();
        }
        const plus = result.instance.exports.plus;
        console.log(plus);

        var x = plus(10_000_000_000, 2);
        console.log(x);
    }).catch(console.error);


separator.addEventListener('mousedown', (e) => {
    isDragging = true;
    document.body.classList.add(whileDraggingClass);
});

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

splitVerticalButton.addEventListener('click', () => {
    setEditorVertical(true, persistOrientationChanges);
});

splitHorizontalButton.addEventListener('click', () => {
    setEditorVertical(false, persistOrientationChanges);
});
