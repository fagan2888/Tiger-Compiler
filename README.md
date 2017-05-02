# Tiger-MIPS Compiler

Written in SML-NJ by Matt Olson and Gabe Kroch for ECE 553 at Duke University. Based on Modern Compiler Implementation by Andrew Appel.

## Information
[Tiger Reference Manual](https://www.lrde.epita.fr/~tiger/tiger.html)

[MIPS Instructions](http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html)

[Sample Tiger Files](https://www.cs.princeton.edu/~appel/modern/testcases/)

## How to Compile
```
CM.make "sources.cm";
Main.compile "file.tig";
```
Creates "file.tig.s" which can be run in SPIM
