# ocaml-instr-freq

Count frequency of basic blocks in OCaml code.
This is useful for knowing which optimizations would have the most impact, if they were to be implemented in the compiler.

Currently, this only works for x86-64 targets.


## Usage example

First, you need to tell the ocaml compiler to save the linear representation of your sources:

```bash
ocamlopt -save-ir-after linearize file.ml
```

If you are using jenga, you can do this via the `OCAMLOPTFLAGS` environment variable:

```bash
OCAMLOPTFLAGS=":standard -save-ir-after linearize" jenga build
```

Next, run this program on the linear file:

```bash
dune exec bin/main.exe -- file.cmir-linear -n-most-frequent 5 -print-n-real-blocks 1 -min-block-size 5 -min-equivalence-class-size 5 -index-file ~/index.bin
```


## Equivalence classes

All basic blocks are different to one another, and so in order to obtain useful
information we group them by equivalence classes.


### Instruction equivalence classes

First, we reason that the two instructions `add_immediate $r0, $r1, 0` and
`add_immediate $r0, $r1, 1` should be considered the same;
as they are performing the same conceptual operation.

From this, we create instruction equivalence classes, where we ignore
constants and other non-essential parts of instructions. To see in detail which
details are ignored, look at the file `lib/types.ml` for fields marked
`[%compare.ignore]`.


### Block equivalence classes

Now that we have created instruction equivalence classes, we can take any block and map all of its instructions
to their equivalence classes.

Furthermore, we also "symbolize" the registers, renumbering them in the order in which they appear.
This is because the two blocks

```asm
movq $r0, $1
movq $r1, $r10
```

and

```asm
movq $r1, $2
movq $r2, $r10
```

should be considered the same, as there is a bijective mapping over registers
which takes you from one block to the other.


With all that's been said so far, two blocks are considered equivalent *if* their instructions are equivalent
and *if* their registers are equivalent.

## Pins required

The tool is currently in an experimental state, and requires specific versions of libraries to function:

```bash
opam pin add ppx_compare https://github.com/janestreet/ppx_compare.git\#58696fd0a9aac7be49fef0ab1ff6798dad3c8a72
opam pin add ppx_hash https://github.com/janestreet/ppx_hash.git\#b69549c05cad09a900e3708c7216761b19dae075
opam pin add ocamlcfg https://github.com/gretay-js/ocamlcfg.git
opam pin add ocaml-migrate-parsetree https://github.com/gretay-js/ocaml-migrate-parsetree.git\#immediate64_ast408
opam pin add ocaml-variants https://github.com/gretay-js/ocaml.git\#wip8
```


## Tests

There are automated tests, based on dune's output diff-ing.

You can run them with:

```bash
dune runtest
```
