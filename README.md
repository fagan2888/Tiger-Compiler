# Tiger-MIPS Compiler

Written in SML-NJ by Matt Olson and Gabe Kroch for ECE 553 at Duke University. Based on Modern Compiler Implementation by Andrew Appel.

## Information
- [Tiger Reference Manual](https://www.lrde.epita.fr/~tiger/tiger.html)
- [MIPS Instructions](http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html)
- [Sample Tiger Files](https://www.cs.princeton.edu/~appel/modern/testcases/)

## How to Compile
```
CM.make "sources.cm";
Main.compile "file.tig";
```
Creates "file.tig.s" which can be run in SPIM

## Issues
- Static link argument when calling functions is not correct, for example for tig_main passes 0($fp) as the static link when it should be $fp. 
- Not handling the $t registers correctly. These are caller saves registers. When calling an external lib function like tig_initarray, the values in the $t registers are lost and therefore some of the test cases fail to run correctly (see test42.tig). 
- Not handling arguments correctly. For example test6.tig which has the do_nothing1 function has 2 arguments, a and b. The b is actually completely ignored and never used. The a is incremented by 1 and then passed as an argument to do_nothing2. Assembly uses argument b instead of a.
