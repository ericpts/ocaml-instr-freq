# ocaml-instr-freq

Count frequency of basic blocks in OCaml code.
This is useful for knowing which optimizations would have the most impact, if they were to be implemented in the compiler.

## Usage example

First, you need to tell the ocaml compiler to save the linear representation of your sources:

```
ocamlopt -save-ir-after linearize file.ml
```


Next, run this program on the linear file:

```
./main.exe file.cmir-linear -n-most-frequent 20 -max-representatives 1 -min-block-size 10 -min-equivalence-class-size 10 -index-file ~/index.bin
```


## Equivalence classes

All basic blocks are different to one another, and so in order to obtain useful information we group them by equivalence classes.


### Instruction equivalence classes
First, we reason that the two instructions `add_immediate $r0, $r1, 0` and `add_immediate $r0, $r1, 1` should be considered the same; 
as they are performing the same conceptual operation.  From this, we create `instruction equivalence classes`, where we ignore the too-minutae details about them.

CR estavarache: Provide a more thorough description of when two instructions are considered equivalent, after we fix the format.

### Block equivalence classes

Now that we have created instruction equivalence classes, we can take any block and map all of its instructions
to their equivalence classes.


## Pins required

The tool is currently in an experimental state, and requires specific versions of libraries to function:

```
opam pin add ppx_compare https://github.com/janestreet/ppx_compare.git\#58696fd0a9aac7be49fef0ab1ff6798dad3c8a72
opam pin add ppx_hash https://github.com/janestreet/ppx_hash.git\#b69549c05cad09a900e3708c7216761b19dae075
opam pin add ocamlcfg https://github.com/gretay-js/ocamlcfg.git
opam pin add ocaml-migrate-parsetree https://github.com/gretay-js/ocaml-migrate-parsetree.git\#immediate64_ast408
opam pin add ocaml-variants https://github.com/gretay-js/ocaml.git\#wip8



```
