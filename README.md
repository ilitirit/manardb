ManarDB
=======

ManarDB is a performant Memory-Mapped storage allocation system based
on the common-lisp object system meta-object-protocol

This is an enhanced fork of the original manardb distribution,
(version designation '0.1.20090911) that provides _support for **non-linux**
platforms_, compatibility with current releases of required libraries,
updates supporting most current common-lisp implementations, and a number of
miscellaneous fixes and feature enhancements.  It does not necessarily
seek to maintain backward compatibility with the API provided by the
original distribution in all cases.
                     

Testing
===

> (asdf:operate 'asdf:load-op 'manardb-test)
> (manardb.test:test-all-manardb)
#<test-run: 24 tests, 3528843 assertions, 0 failures (0 expected) in 30.306 sec>

Credits
===

Based on [ManarDB version 0.1.20090911](http://cl-www.msi.co.jp/projects/manardb/index.html)
by [Mathematical Systems Incorporated](http://www.msi.co.jp).

- Thanks to John Fremlin for a nice platform on which to hack and extend.
- Thanks to Pascal Costanza for MOP conformance and other advice.
