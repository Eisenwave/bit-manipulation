export const container = document.getElementById('container');

export const editorFractionLimit = 0.1;
export const editorFractionItem = 'editorFraction';
export const editorIsVerticalItem = 'editorIsVertical';

let isVertical = false;

export function resizeContainerToFraction(f) {
    const value = `${f}fr var(--separator-width) ${1 - f}fr`;
    if (isVertical) {
        container.style.gridTemplateRows = value;
    } else {
        container.style.gridTemplateColumns = value;
    }
}

/**
 * Swaps the `gridTemplateColumns` and `gridTemplateRows` in a style.
 * @param {CSSStyleDeclaration} e 
 */
function swapGridTemplateRowsColumns(e) {
    let temp = e.gridTemplateColumns;
    e.gridTemplateColumns = e.gridTemplateRows;
    e.gridTemplateRows = temp;
}

export function isEditorVertical() {
    return isVertical;
}

export function setEditorVertical(vertical, persist = false) {
    if (isVertical == vertical) {
        return;
    }
    if (vertical) {
        document.body.classList.add('vertical');
    } else {
        document.body.classList.remove('vertical');
    }
    swapGridTemplateRowsColumns(container.style);
    isVertical = vertical;
    if (persist) {
        localStorage.setItem(editorIsVerticalItem, vertical);
    }
}

const initialFraction = localStorage.getItem(editorFractionItem);
if (initialFraction !== null) {
    resizeContainerToFraction(initialFraction);
}

const initialIsVertical = localStorage.getItem(editorIsVerticalItem);
if (initialIsVertical !== null) {
    setEditorVertical(initialIsVertical);
}
