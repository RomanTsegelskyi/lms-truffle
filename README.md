Combining LMS and Truffle framework
==================

What do we get if we combine self-modifying AST interpreters ([Truffle](http://www.christianwimmer.at/Publications/Wimmer12b/)) 
and generative programming ([LMS](http://scala-lms.github.io))?

This project is based on previous work by Prof. Rompf - [Truffle Playground](https://github.com/TiarkRompf/truffle-playground) and is a final project for CS590 Fall 2014

### Background

- [Truffle FAQ and Guidelines](https://wiki.openjdk.java.net/display/Graal/Truffle+FAQ+and+Guidelines#TruffleFAQandGuidelines)

- [Truffle API tests](http://hg.openjdk.java.net/graal/graal/file/483d05bf77a7/graal/com.oracle.truffle.api.test/src/com/oracle/truffle/api/test)

- [JRuby/Truffle](http://www.chrisseaton.com/rubytruffle/), [code](https://github.com/jruby/jruby/wiki/Truffle)

### Running

Grab a [Graal VM binary](http://lafo.ssw.uni-linz.ac.at/builds/). I'm using (GraalVM-0.2 OpenJDK-8-b132)[].
Remember to set `JAVA_HOME`. Use `sbt` to compile and run.

### License

GPLv2

