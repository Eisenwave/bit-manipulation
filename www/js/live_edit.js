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
