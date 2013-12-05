ParseGen
========

ParseGen is rule based parser generator for [FPC](http://www.freepascal.org) / [Lazarus](http://www.lazarus.freepascal.org). ParseGen is design using FPC and Lazarus and it mainly supports for Windows and Linux operating systems. 

ParserGen scanner is slight DFA (deterministic finite automaton) state machine. This scanner can operate on case sensitive or case-insensitive modes and it can easily configure/extend through given class structure. The parser of the ParseGen is recursive descent parser. ParseGen generate this parser using high-level source language and this language is logically similar to EBNF grammar.

This parser generator is mainly design for  FPC / Lazarus, but it can use with Delphi with some minor changes. ParseGen is an open source software project and it is based on [GNU General Public License
 version 3](http://www.gnu.org/licenses/gpl.html).

Most of the sample scripts and demo applications of this project are based on [Niklaus Writh's](http://www.inf.ethz.ch/personal/wirth) *Algorithms + Data Structures = Programs* book and *PL/0* programming language. In demo project we implement Writh's *PL/0* parser using ParseGen and it is available in main project bundle. 
