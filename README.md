LMS-Truffle: combining staging with self-optimizing AST interpretation
==================

What do we get if we combine self-modifying AST interpreters ([Truffle](http://www.christianwimmer.at/Publications/Wimmer12b/)) 
and generative programming ([LMS](http://scala-lms.github.io))?

The main goal of this project is to proved that needed machinery for using Truffle Nodes being a target for some typical uses cases of Lightweight Modular Staging and evaluate the performance. 
This project is based on previous work by Prof. Rompf - [Truffle Playground](https://github.com/TiarkRompf/truffle-playground) and is a final project for CS590 Fall 2014

### Background

- [Truffle FAQ and Guidelines](https://wiki.openjdk.java.net/display/Graal/Truffle+FAQ+and+Guidelines#TruffleFAQandGuidelines)

- [Truffle API tests](http://hg.openjdk.java.net/graal/graal/file/483d05bf77a7/graal/com.oracle.truffle.api.test/src/com/oracle/truffle/api/test)

- [Lightweight Modular Staging](http://scala-lms.github.io)

### Truffle Version

I am using Truffle 0.5v.

### Running

Get a  [Graal VM binary](http://lafo.ssw.uni-linz.ac.at/builds/).
Use `sbt` to compile.
Use `sbt test` to run tests.

### License

GPLv2

