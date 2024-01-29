# Bit Manipulation Wiki

This project is intended to document bit manipulation functions in a comprehensive,
well-formatted, easily referenced document.

## Contributing

This project uses two simple domain-specific languages.

1. **Bit Manipulation Doc** is a markup language vaguely similar to TeX.
   It pretty much directly translates into HTML tags, but is much nicer to work with than
   directly writing HTML.

2. **Bit Manipulation Script** is a simple C-style/Rust-style programming language.
   If you know C, you almost know BMS.
   While it is a formal language, it is extremely simple and restricted so that readers with
   practically no prior knowledge can comprehend it.




### Bit manipulation doc (BMD)

BMD consists of only a small amount of elements.
1. Plain text, where empty lines separate paragraphs.
2. `/* block comments */` and `// line comments`
4. `\elements`, `\elements{...}` and `\elements[a = ..., b = ...]{...}`

BMD largely translates 1:1 into HTML, although it is more convenient to write.
For example, BMD translates into HTML as follows:
```tex
Hello, \emph{world}!

Second paragraph.
```
```html
<p>Hello, <i>world</i>!</p>
<p>Second paragraph</p>
```

You should be able to get up and running with writing BMD files within a few minutes.
Just take a look at existing examples in this repository and see the reference below.

#### BMD Elements

##### Comments

Comments can be defined C-style:
- `// line comments`, ending with the line
- `/* block comments */`, possibly spanning multiple lines

##### Headings

Headings can be defined using `\h1`, `\h2`, `h3`, `\h4`, and `\h5`, where `\h1` is the
top-level heading, only to be used once on the page.

A typical structure would look like:
```tex
\h1{Title}

Introduction ...

\h2{Subtitle}

Stuff ...
```

##### Paragraphs

An empty line between prose implicitly splits paragraphs.
For example:
```tex
Paragraph A.

Paragraph B.
```
Translates into
```html
<p>Paragraph A.</p>
<p>Paragraph B.</p>
```

The contents of certain elements are exempt from this; for example, code blocks, block quotes,
and other blocks are always one paragraph.

##### Text style

| BMD | Meaning |
| --- | ------- |
| `\emph{text}` | `text` is emphasized, i.e. *italic*
| `\bold{text}` | `text` is **bold**
| `\uline{text}` | `text` is __underlined__
| `\strike{text}` | `text` is ~~strikethrough~~
| `\code[lang=bms]{text}` | `text` is `code` with `bms` highlighting

Note: `\code{text}` is equivalent to `\code[lang=bms]{text}`.
      Language attributes are only necessary to change syntax highlighting.

##### Text color

It is possible to color text to a limited extent.
Colors must first be defined in the document using `\defcolor[id=..., color=#ff0000]`.
The `\defcolor` element has the following attributes:
- `color` is a CSS color which sets the color for light themes and dark themes
- `id` is the color identifier, with which the color is referenced later
- `dark` is an optional attribute which sets the color for dark themes

Text can be given color using defined colors.
For example, `\color[id=red]{text}` makes `text` red, if `red` was previously defined.

##### Lists

Lists can be defined as follows:
```tex
\enum{
    \item{first}
    \item{second}
    \item{third}
}
\list{
    \item{a}
    \item{b}
    \item{c}
}
```
This translates into
```html
<ol>
    <li>first</li>
    <li>second</li>
    <li>third</li>
</ol>
<ul>
    <li>a</li>
    <li>b</li>
    <li>c</li>
</ul>
```
A `\list` or `\enum` element can only contain `\item`, `\enum`, or `\list` elements.

##### Code blocks

Code can be nested inside documents using:
```tex
\block[lang=bms]{
function identity(x: Uint(N)) -> Uint(N)
{
    // basic test program
    return x;
}
}
```

`\block`s have the following attributes:

- `lang` specifies the language for syntax highlighting.
   If none, the block is highlighted as `bms`.
   This must currently be one of: `bms`, `text`, `c`, `cpp`, `c++`, `rust`, `asm`

Syntax highlighting is all done in BMD, not in the front-end.
The support for anything but BMS is limited to tokenizer-based highlighting.
Keywords and special characters are recognized, but not much more.

##### Math

Math formulas can be nested with `\math` through MathJax and use TeX syntax:
```tex
\math{\sum_{n=0}^N{n}}
```

##### Block-Quotes

Block-quotes can be nested similar to code blocks:
```tex
\quote[src=...]{
    This is a quote.
}
```
The `src` attribute is optional and refers to the source of the quote, defined by a `\src` element.

##### References

Other sections in the document and external sources can be referenced using `\ref[id=...]`,
or `\ref[id=...]{text}` where `text` turns into a masked link.

For each reference, you must create a source with `\src` somewhere in the document:
```tex
\src[
    id = source_id,
    ref = "https://example.com",
    title = "Title of the source",
    author = "Example Author",
]
```
Only the `id` and `ref` attribute are mandatory.


##### HTML passthrough

If the native features are not sufficient, HTML can also be generated with `\html_` elements:
```tex
\html_div[id=test]{
    Text.
}
```
Translates into
```html
<div id="test">
    Text.
</div>
```



### Bit manipulation script (BMS)

BMS is a simple statically typed language that borrows elements mostly from C and Rust.
This language exists solely to document bit manipulation operations, so it is intentionally
limited in many ways.
For example, operator associativity doesn't exist because expressions must be nested in
parentheses whenever there would be some ambiguity.

BMS can be used to generate more-or-less equivalent code in other languages.
This is also the motivation for developing a language for this document.
Other languages have their own features and oddities that make them difficult to automatically
translate.


#### Example

```js
// Define a function named "reverse".
// This function has a parameter "x" of type "Uint(N)".
// Uint(N) is an unsigned integer with N bits.
// N is a generic constant, so the function is like a template which works for any it size.
// This function returns a Uint(N).
function reverse(x: Uint(N)) -> Uint(N)
  requires true // requires-clause for preconditions (optional, just here for demonstration).
{
    // Copy x into a variable so we can do some modifications.
    let normal = x;
    // Define a local variable of type of type Uint(N), with value zero.
    let reversed: Uint(N) = 0;
    // Defined a local variable fo type Int, with value zero.
    let i: Int = 0;
    // Loop as long as i is less than N.
    while i < N {
        // Assign result to result, bit-shifted to the left by one.
        reversed = reversed << 1;
        // Assign result to result, bitwise-ORed with the bitwise AND of x and 1.
        reversed = reversed | (normal & 1);
        // Assign x to x, (logically) right-shifted by one.
        normal = normal >> 1;
        // Assign i to i, incremented.
        i = i + 1;
    }
    // The result of the function is the current value of result.
    return reversed;
}
```

#### Programs

BMS programs contain a sequence of one of the following:

1. Comments (can appear anywhere, and won't be mentioned in the rest of this documentation).
2. Functions definitions.
3. Constants.

#### Types

Any value has an associated type, which is one of:
- `Void`, a type with only one value.
- `Bool`, `true` or `false`.
- `Int`, an infinite-precision integer.
- `Uint(N)`, an N-bit unsigned integer.

#### Literals

Values either come from function inputs, or stem from literals one way or the other.
`Bool` values can be `true` or `false`.
`Int` can be defined using any integer literal:
- `0b101010` for binary literals.
- `073` for octal literals. (notice the leading zero)
- `123` for decimal literals.
- `0xff` for hexadecimal literals.

All integer literals are implicitly of type `Int`.

#### Type conversions

`Int` can be implicitly converted to `Uint`.
However, it is *erroneous* to convert an `Int` to a `Uint` if the result cannot be represented
in the destination.
For example:
```js
const x = 1000;
const big: Uint(16) = x; // OK
const small: Uint(8) = x; // erroneous
```
If the BMS interpreter spots such an erroneous conversion, it halts.


#### Variables and constants

Constants can be of the following form:
```js
const x = EXPRESSION;
const x: TYPE = EXPRESSION;
```
Variables can be defined like constants, just with `let` instead of `const`.
Unlike constants, variables can be re-assigned to a different value,
and they aren't required to be known prior to evaluation.

`const` means that the declared object is a constant, i.e. its value must be known
prior to the evaluation of the function.

Furthermore, variables can be left uninitialized:
```js
let x: Int;
x = 1; // assign later
```
Reading the value of a variable that has not been initialized is *erroneous*.

Here are some more examples:
```js
const x = N / 2; // OK, x depends on generic parameter
const x: Uint(32) = 0; // OK, y is initialized by a literal
let y = /* ... */;
const z = y;    // error: z must be initialized by a constant expression
```

#### Assignments

Variables (but not parameters) can be assigned using the syntax
```js
x = /* expression */;
```
Multi-assignments are **not allowed** (`x = y = ...`).

#### Functions

The main unit of interest is a function.
A function takes some amount of inputs and produces one output as a result.

##### Parameters and return types

```js
function f(x: Int, y: Bool) -> Bool // ...
```
This function takes two inputs `x` and `y` of types `Int` and `Bool` respectively.
It returns a value of type `Bool`.

The values of function parameters can be accessed within the function body.
However, parameters cannot be assigned.

**Rationale:** Not every language has mutable function parameters by default, which makes the
act of assigning them possibly surprising.

##### Bit-generic functions

If a parameter has type `Uint` where the width of the integer is an identifier that is not yet
defined, the function is bit-generic:
```js
const M = 8;
// not bit-generic because M is a constant
function f(x: Uint(M)) -> Void // ...

// bit-generic because N is not yet defined
function g(x: Uint(N)) -> Void
```
`N` can be used as a constant expression within the function.

Most functions in this document are bit-generic.
By substituting the bit-generic parameter for a concrete constant,
non-generic code in other languages can be generated.

**Rationale:** Bit-generic functions are necessary to express operations for any bit width.
This is the minimum degree of generics that the language needs.

##### Requires-clauses

A function can have a requires-clause to document constraints.
These constraints are preconditions which must be met for a function to be called.

```js
function f(x: Uint(N)) -> Uint(N)
  requires is_power_of_two(N)
{ /* ... */ }
```

**Rationale:** A lot of algorithms only work for bit-widths of powers of two, and a standardized
way of constraining them is convenient.

#### Expressions

To actually perform computations, you need expressions.
For example:
```js
let x = a + 1;
```
Expressions in BMS are extremely limited so as to not surprise the reader.
Almost all operators from C are supported, however:
- There are no pointers, so there is no address-of or indirection.
- Operators cannot be chained. For example, `10 + 20 + 30` is ill-formed and must be written as
  `(10 + 20) + 30`.
  Therefore, there is no associativity and there is no precedence between different binary
  or unary operators.
- There is no compound assignment or increment (`++x`, `x += 1`, etc.).
  The only way to change the value of a variable is to use an assignment.
- Assignments are not expressions, i.e. `x + (y = 0)` is illegal.
- There is no comma operator.

**Rationale:** Every language has its own quirks when it comes to precedence, the specific
operators supported, etc.
BMS must be easily understood, and this very basic form is a common subset found in all modern
languages.

Furthermore, certain operators can only be applied to values of certain types:
- Bitwise operators can only be applied to `Uint`.
- Logical operators (`not`, `or`, `and`) can only be applied to `Bool`.
  There is no type coercion like in C, i.e. `if 0` is not the same as `if false`.

**Rationale:** Each language has its own quirks when it comes to mixed-sign comparisons,
type coercions, etc.
It is important to prevent any confusion by making the behavior explicit.

#### If-expressions

Most languages have some form of conditional operator.
In BMS, a Python-style form is used:
```js
// BMS
let x = a if /* condition */ else b;
// C
int x = /* condition */ ? a : b;
// Rust
let x = if /* condition */ { a } else { b };
```

**Rationale:** The syntax is concise and can be nicely pronounced in English.
Since the condition is sandwiched between two keywords, there is also no confusion as to
precedence.
By comparison, C's `1 + 0 ? a : b` could be either `a` or `b` depending on the precedence of `?:`
in relation to `+`. 

##### Nesting if-expressions

If-expressions cannot be nested due to the mental overhead and the confusion this would create.
Remember that BMS is a language to be intuitively understood at first glance.

Using parentheses, it is still possible to write:
```js
let x = a if (true if /* condition */ else false) else b;
```
A parenthesized expression can be used in stead of `a` or `b` as well.