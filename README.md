# ocaml-instr-freq

[![Build Status](https://travis-ci.org/ericpts/ocaml-instr-freq.svg?branch=master)](https://travis-ci.org/ericpts/ocaml-instr-freq)

Count frequency of basic blocks in OCaml code.
This is useful for knowing which optimizations would have the most impact, if they were to be implemented in the compiler.

Currently, this only works for x86-64 targets.


## Usage example

In this part, we will be using the test fixture `repetitive.ml`:

```ml
let rec f0 n =
  match n with
  | 0 | 1 -> 1
  | n -> f0 (n - 1) + f0 (n - 2)
;;

let rec f1 n =
  match n with
  | 0 | 1 -> 2
  | n -> f1 (n - 1) + f1 (n - 2)
;;

let rec f2 n =
  match n with
  | 0 | 1 | 2 -> 2
  | n -> f2 (n - 2) + f2 (n - 3)
;;
```

First, you need to tell the ocaml compiler to save the linear representation of your sources:

```bash
ocamlopt -save-ir-after linearize test/fixtures/repetitive.ml
```

If you are using dune, you can do this via the `OCAMLOPTFLAGS` environment variable:

```bash
OCAMLOPTFLAGS=":standard -save-ir-after linearize" dune build
```

Next, run this program on the linear file:

```bash
dune exec bin/main.exe -- test/fixtures/repetitive.cmir-linear -index-file /tmp/index.bin
```

and you should get output which looks like this (but in your terminal it should be
nicely colored):

```asm
There are a total of 3 blocks matching the query criteria.
Equivalence <1> with 3 members;
	file: test/fixtures/repetitive.cmir-linear
	function: camlRepetitive__f0_11
Blocks (120): {
	.file ""
	.section .text._fun_start_,"ax",@progbits
	.align	16
	.globl	_fun_start_
_fun_start_:
	.cfi_startproc
.L120:
	movq	%rax, (%rsp)
	addq	$-4, %rax
	call	camlRepetitive__f0_11@PLT
.L100:
	movq	%rax, 8(%rsp)
	movq	(%rsp), %rax
	addq	$-2, %rax
	call	camlRepetitive__f0_11@PLT
.L101:
	movq	8(%rsp), %rbx
	addq	%rbx, %rax
	decq	%rax
	ret
	.cfi_endproc
	.type _fun_start_,@function
	.size _fun_start_,. - _fun_start_
==========
.L120:
	(Op Spill): Arg(reg#0) Res(stack#local#0) Live(reg#0)
	(Op (Intop_imm Iadd -4)): Arg(reg#0) Res(reg#0) Live(stack#local#0)
	(Call (F (Immediate (func camlRepetitive__f0_11) (label_after 100)))): Arg(reg#0) Res(reg#0) Live(stack#local#0)
	(Op Spill): Arg(reg#0) Res(stack#local#1) Live(stack#local#0)
	(Op Reload): Arg(stack#local#0) Res(reg#0) Live(stack#local#1)
	(Op (Intop_imm Iadd -2)): Arg(reg#0) Res(reg#0) Live(stack#local#1)
	(Call (F (Immediate (func camlRepetitive__f0_11) (label_after 101)))): Arg(reg#0) Res(reg#0) Live(stack#local#1)
	(Op Reload): Arg(stack#local#1) Res(reg#1) Live(reg#0)
	(Op (Intop Iadd)): Arg(reg#0 reg#1) Res(reg#0) Live()
	(Op (Intop_imm Iadd -1)): Arg(reg#0) Res(reg#0) Live()
	Reloadretaddr: Arg() Res() Live()
	#-Return-# Arg(reg#0) Res() Live() 
}
```

This printed out a single basic block, with label 120.
The first part (before =========) is its assembly form, whereas the second part (after =======) is the CFG format.


See [here](#equivalenceclasses) for how blocks are grouped together.


In the CFG format, the instruction is followed by register information: `Arg` are the input registers (where they are indexed, so `reg#0` is `%rax`, and `stack#local#0` is `(%rsp)`.), `Res` are the output registers, and `Live` are the live ones (i.e., their value is used at some point after the current instruction).

By default, this only prints out blocks of length (number of linear instructions) of at least 5 (including the terminator). For a different value, pass the `-min-block-size <num>` flag.

### Index caching

Note that this will save the index to `/tmp/index.bin`, and then reuse it if you rerun the program.
This is intended for most use cases, when you are exploring a large code base and want to save time, however
if you change the files, or the `-context` option, then you need to remove the stale index manually.

### Assembly

The printed assembly is only an approximation of the final output, which it should be
very very close to, but not identical. Most of the changes come in the form of which labels are generated (i.e., the final code may have a different labelling scheme), 
and in the layout of the code (`ja BL1; BL2: [...]` vs `jbe BL2; BL1: [...]`), but the
actual instructions generated should be the same.

With that said, the printed `CFG` is the final one, used by the compiler itself, so it can be fully trusted.

The assembly form contains some preamble and postamble, but the relevant code lays 
between `.cfi_startproc` and `.cfi_endproc`.

## Context

It is possible to increase the "context". By default, the program looks at individual basic blocks, which is a context of 0.

A context length of 1 corresponds to looking at pairs of adjacent basic blocks of the cfg,
and you can make this length arbitrarily long, though at the cost of (possibly) exponential explosion.

Let us look at the same file, `repetitive.ml`, with a context length of 1.

```bash
rm -f /tmp/index.bin # remove the old, stale index
dune exec bin/main.exe -- test/fixtures/repetitive.cmir-linear -index-file /tmp/index.bin  -context 1
```

and you should get output which looks like this:

```asm
Equivalence <0> with 3 members;
	file: test/fixtures/repetitive.cmir-linear
	function: camlRepetitive__f0_11
Blocks (0 120): {
	.file ""
	.section .text._fun_start_,"ax",@progbits
	.align	16
	.globl	_fun_start_
_fun_start_:
	.cfi_startproc
.L0:
.L123:
	cmpq	$3, %rax
	jbe	.L102
.L120:
	movq	%rax, (%rsp)
	addq	$-4, %rax
	call	camlRepetitive__f0_11@PLT
.L100:
	movq	%rax, 8(%rsp)
	movq	(%rsp), %rax
	addq	$-2, %rax
	call	camlRepetitive__f0_11@PLT
.L101:
	movq	8(%rsp), %rbx
	addq	%rbx, %rax
	decq	%rax
	ret
	.cfi_endproc
	.type _fun_start_,@function
	.size _fun_start_,. - _fun_start_
==========
.L0:
	Prologue: Arg() Res() Live()
	#-(Branch
	    (((Test (Iinttest_imm (Iunsigned Cle) 3)) 102)
	        ((Test (Iinttest_imm (Iunsigned Cgt) 3)) 120)))-# Arg(reg#0) Res() Live() 
.L120:
	(Op Spill): Arg(reg#0) Res(stack#local#0) Live(reg#0)
	(Op (Intop_imm Iadd -4)): Arg(reg#0) Res(reg#0) Live(stack#local#0)
	(Call (F (Immediate (func camlRepetitive__f0_11) (label_after 100)))): Arg(reg#0) Res(reg#0) Live(stack#local#0)
	(Op Spill): Arg(reg#0) Res(stack#local#1) Live(stack#local#0)
	(Op Reload): Arg(stack#local#0) Res(reg#0) Live(stack#local#1)
	(Op (Intop_imm Iadd -2)): Arg(reg#0) Res(reg#0) Live(stack#local#1)
	(Call (F (Immediate (func camlRepetitive__f0_11) (label_after 101)))): Arg(reg#0) Res(reg#0) Live(stack#local#1)
	(Op Reload): Arg(stack#local#1) Res(reg#1) Live(reg#0)
	(Op (Intop Iadd)): Arg(reg#0 reg#1) Res(reg#0) Live()
	(Op (Intop_imm Iadd -1)): Arg(reg#0) Res(reg#0) Live()
	Reloadretaddr: Arg() Res() Live()
	#-Return-# Arg(reg#0) Res() Live() 
}


Equivalence <1> with 3 members;
	file: test/fixtures/repetitive.cmir-linear
	function: camlRepetitive__f0_11
Blocks (0 102): {
	.file ""
	.section .text._fun_start_,"ax",@progbits
	.align	16
	.globl	_fun_start_
_fun_start_:
	.cfi_startproc
.L0:
.L124:
	cmpq	$3, %rax
	ja	.L120
.L102:
	movq	$3, %rax
	ret
	.cfi_endproc
	.type _fun_start_,@function
	.size _fun_start_,. - _fun_start_
==========
.L0:
	Prologue: Arg() Res() Live()
	#-(Branch
	    (((Test (Iinttest_imm (Iunsigned Cle) 3)) 102)
	        ((Test (Iinttest_imm (Iunsigned Cgt) 3)) 120)))-# Arg(reg#0) Res() Live() 
.L102:
	(Op (Const_int 3)): Arg() Res(reg#0) Live()
	Reloadretaddr: Arg() Res() Live()
	#-Return-# Arg(reg#0) Res() Live() 
}
```

Notice that this time, each printout is composed of two basic blocks glued together; the first one is made of 0 and 120, and the second one of 0 and 102, and the layout is different, but the code contained inside is the same, and the CFG for block 0 is the same in both.


### Matchers


#### Subsequence

If you identify an interesting pattern, and want to see how many times it appears, you can use
the matching functionality.

For this case, we will use the `test/fixtures/long_function.ml` example.
Using a context of 1,  let's say that we observe the following sequence in the cfg code:

```
        (Op (Intop_imm Iadd -2)): Arg(reg#1) Res(reg#1) Live(stack#local#1 stack#local#2 stack#local#3 stack#local#4 reg#4 reg#0)
		
        #-(Branch
            (((Test (Iinttest (Isigned Cgt))) 101)
                ((Test (Iinttest (Isigned Cle))) 127)))-# Arg(reg#4 reg#1) Res() Live()

.L127:
        (Op Spill): Arg(reg#1) Res(stack#local#0) Live(stack#local#1 stack#local#2 stack#local#3 stack#local#4 reg#4 reg#0)
```

which corresponds to assembly

```
        addq    $-2, %rbx
        cmpq    %rbx, %rdx
        jg      .L101
.L127:
        movq    %rbx, (%rsp)
```

and we would like to see how many times this pattern repeats.

This matching is done modulo constants, such as the labels involved, the immediate
values, and so on.

To search for this pattern, we need to compose a file containing the matching pattern:


``` 
$ cat matcher.sexp
(
 (desc (Basic (Op (Intop_imm Iadd -2))))
 (arg (0))
 (res (0))
)

(
 (desc (Terminator 
        (Branch
         (((Test (Iinttest (Isigned Cgt))) 101)
          ((Test (Iinttest (Isigned Cle))) 127)))))
 (arg (1 0))
 (res ())
)

(
 (desc (Basic (Op Spill)))
 (arg (0))
 (res (2))
)
```

where each of the original cfg instructions now became a sexp, where register information has been moved to separate fields. 

Pattern matching works as a "subsequence" search: a pattern matches a block iff there exists a subsequence of the block which is isomorphic to the pattern (modulo register renaming and instruction constants).

Note that in this syntax, registers are not exact, but rather they represent sameness.
In our example, we enforce that the same register is used in the `Iadd` operation, in
the terminator condition, and in the spill which comes right afer.

Also see that this matcher example crosses basic block boundaries, so it needs to be ran with context >= 1.


After this, we can run the command

```
rm -f /tmp/index.bin
dune exec bin/main.exe -- test/fixtures/long_function.cmir-linear -index-file /tmp/index.bin -context 1 -use-subsequence-matcher ~/matcher.sexp
```

and see that there is a single occurance of this pattern.


#### Whole block

In case the subsequence matchers are not precise enough, you are able to define whole-block matchers, with
your custom predicate.

Say we want to create a pattern matcher which just looks for `add` instructions; let's call this
matcher `simple_add`.

We then create the file `patterns/pattern_simple_add.ml`, and then pass it to the program by via the `-use-whole-block-matcher` flag:


```
rm -f /tmp/index.bin
dune exec bin/main.exe -- test/fixtures/long_function.cmir-linear -index-file /tmp/index.bin -context 1 -use-whole-block-matcher simple_add
```

## <a name="equivalenceclasses"> Equivalence classes </a>

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
