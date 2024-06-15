#lang scribble/manual

@(require scribble/core)
@(require scribble/html-properties)
@(define c tt)
@(define (ext-image src)
  (make-element
   (make-style #f
               (list (make-alt-tag "img")
                     (make-attributes `((src . ,src)))))
   ""))


@title{Kittle Buffer}

@ext-image{https://res.cloudinary.com/kdr2/image/upload/c_scale,w_128/v1619321978/dev/kbf-icon-1024.png}

@section{Introduction}

Kittle Buffer, or KBF for short, is an extended
@hyperlink["https://en.wikipedia.org/wiki/Brainfuck"]{Brainf**k}
implementation. KBF could also be explained as Killian’s Brainf**k.

@margin-note{This is a project I use to teach my kid, that's why I
renamed it to eliminate the dirty word.}

This document doesn't contain any introduction or manual of BF, but
you must know what BF is before you use KBF. BF has only 8
instructions, it is a simple yet intresting programming language, you
will easily find many materials for learning it if you do a search on
the internet.

@section{Extensions to the vanilla BF}

I added a few extensions to the vanilla BF with the intention of
making it more expressive, more interesting, and richer for kid
teaching.

Among these extensions, the most important ones are @c{^} and
@c|{@}|.

@c{^} is for multiple pointers support. A pointer stack is added
besides the data cells (a.k.a, the data buffer), you can push into or
pop out a pointer from the pointer stack. Each pointer has a integer
ID. When a BF program starts, the default pointer whose ID is 0 is
pushed into the pointer stack automatically. Afterwards, you can push
a pointer into the pointer stack by @c{^ID} where ID is the
numerical ID of the pointer. If you want to pop the top pointer out
from the pointer stack, just use @c{^} without any digit following
it. As a special case, the last pointer in the pointer stack can't be
popped out: @c{^} does nothing when there's only one pointer in the
pointer stack.

The current pointer which instrcutions like @c{+-<>} are
manipulating is always the top pointer of the pointer stack.

@c|{@}| is added for function call. The vanilla BF has no function
related facilities and it is hard to do calculations like comparison
and division. To cope with this issue, I add a function table to
KBF. Each function in that table has an integer ID, for example:

@itemlist[

@item{@c{0} coresponds to function @c{identify}, a simple
function returns its only argument untouched.}

@item{@c{1} converts its argument to a string and output it.}

@item{@c{21} is @c{+} who adds all its arguments up.}

]

A function call is usually expressed as @c{ret = func(args...)} in
common programming languages, so do we here:

@itemlist[

@item{first, we use @c{^N} to push a pointer into the pointer
stack, the data cell to which this pointer points to will be used to
hold the return value of he function, this step can be thought as
@c{ret =}}

@item{then, again we use @c{^N} to push the pointer who points to
the data cell with the target function id in it into the pointer
stack, this step coresponds to @c{func}.}

@item{then, at push the pointer(s) to the arguments orderly.}

@item{at last, we use @c|{@ARITY}| to trigger the function call,
  here @c{ARITY} is count of the arguments.}

@item{@c|{@ARITY}| doesn't change the pointer stack, you should pop
  out the pointers manually if needed.}

]

Let's see an example: @c|{^0^1>+21^2>>+^3>>>++@2}| calculates that
@c{cell(0) = cell(2) + cell(3)}, which is @c{cell(0) = 1 + 2}.

@itemlist[

@item{@c{^0} pushes pointer 0 to the pointer stack, which is for the
return value.}

@item{@c{^1} pushes pointer 1 to the pointer stack, @c{>} moves
it right to cell(1), @c{+21} adds it up to @c{21}. This is for
function @c{+}.}

@item{@c{^2>>+^3>>>++} prepares the arguments, @c{1} in cell(2),
@c{2} in cell(3).}

@item{@c|{@2}| calls the function, after this step, cell(0) will
become @c{3}.}

]

There are also some trivial extensions besides the two main
extensions:

@itemlist[

@item{Using a number for repetition, @c{+23} means repeating
@c{+} for 23 times. This works for @c{+-<>};}

@item{@c{!} for input data, like what
@hyperlink["https://fatiherikli.github.io/brainfuck-visualizer/"]{this
BF visualizer} does (sadly this is broken now). The characters after a
@c{!} will not be treated as code, but as data. @c{,} will pick
character from this data @bold{circularly}.}

@item{@c{?} to set a break-point under GUI mode. Under cli-mode, it
does nothing, but under GUI mode, it will pause when an @c{?}
instruction is encountered if the "Pause at ?" checkbox is checked.}

]

Here is a simple demonstration of the GUI window:

@ext-image{https://res.cloudinary.com/kdr2/image/upload/v1617707587/dev/kbf-v0.2-alpha.gif}

@section{How to obtain KBF}

There are @hyperlink["https://cxan.kdr2.com/kittle-buffer/"]{pre-built
binaries hosted here}:

@itemlist[

@item{@hyperlink["https://cxan.kdr2.com/kittle-buffer/kittle-buffer-linux-v0.2.zip"]{Linux}}

@item{@hyperlink["https://cxan.kdr2.com/kittle-buffer/kittle-buffer-win64-v0.2.1.zip"]{Windows}}

@item{@hyperlink["https://cxan.kdr2.com/kittle-buffer/kittle-buffer-macos-v0.2.1.zip"]{macOS}}

]

Or you can
@hyperlink["https://github.com/KDr2/kittle-buffer"]{download the
source from github} and run it from source:

@itemlist[

@item{install racket}

@item{clone the source, run ~raco pkg install~ in the project
direcotry}

@item{run @c{kbf.rkt}

@itemlist[

@item{@c{racket kbf.rkt} will start with the GUI;}

@item{@c{racket kbf.rkt src.bf} will run the BF/KBF code in the
file @c{src.bf}};

@item{@c{racket kbf.rkt -} will read BF/KBF source from
@c{stdin}.}

]
}

]

@section{The builtin function-table}

@itemlist[

@item{0: @c|{@1}|, identify, a function returns its only argument
untouched.}

@item{1: @c|{@1}|, outputs the number literally, instead of
converting it to a char, returns the count of chars it has printed
out.}

@item{21: @c|{@N}| (N>=1), +}

@item{22: @c|{@N}| (N>=1), -}

@item{23: @c|{@N}| (N>=1), *}

@item{24: @c|{@2}|, /}

@item{25: @c|{@2}|, modulo}

@item{26: @c|{@2}|, power, returns arg-0 raised to the power of
arg-1.}

@item{27: @c|{@2}|, =, returns 0 or 1 which stands for false and
true respectively.}

@item{28: @c|{@2}|, >}

@item{29: @c|{@2}|, <}

@item{30: @c|{@2}|, >=}

@item{31: @c|{@2}|, <=}

@item{32: @c|{@2}|, bitwise or}

@item{33: @c|{@2}|, bitwise and}

@item{34: @c|{@2}|, bitwise xor}

@item{35: @c|{@2}|, bitwise shift}

@item{36: @c|{@2}|, random}

]

@section{Changelog}

@bold{v0.2.1}

@itemlist[

@item{Change icon on macOS and Windows.}

]

@bold{v0.2}

@itemlist[

@item{Use @c{?} to set a break-point under GUI mode.}

@item{Colorize related cells while executing a function call
instruction (i.e., @c|{@N}|).}

@item{Cacth the error when brackets mismatch.}

]

@bold{v0.1}

@itemlist[

@item{The first release}

@item{An extended BF interpreter with the following extensions:
@c|{^, @, <N>, !}|.}

@item{A graphic user interface to visualize the execution process of
BF.}

]

@section{Examples}

The below code:
@codeblock|{
  ^0^1>+21^2>>+50^3>3+47@2^0.
}|

outputs @c{a}.

@itemlist[

@item{@c{^0} pushes the pointer 0 to the pointer stack;}

@item{@c{^1>+21} pushes the pointer 1 to the pointer stack, moves
it right for 1 step, then adds its value to 21 (21 is the function
@c{+});}

@item{@c{^2>>+50} pushes the pointer 2 to the pointer stack, moves
it right for 2 steps, then adds its value from 0 to 50;}

@item{@c{^3>3+47} pushes the pointer 3 to the pointer stack, moves
it right for 3 steps, then adds its value from 0 to 47;}

@item{@c|{@2}| uses the top 2 pointers (^2 and ^3) as arguments,
calls the function 21 to which the pointer 1 is pointing, places the
return value (97 here) into the cell to which pointer 0 is pointing;}

@item{@c{^0.} selects pointer 0 and outputs the value it points
to.}

]
