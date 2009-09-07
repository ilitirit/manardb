manardb is a portable (across Lisps on Linux) memory mapped database for Common Lisp.
===
It frees one from the garbage collector but stays within MOP.

Testing
===

> (asdf:operate 'asdf:load-op 'manardb-test)
> (manardb.test:test-all-manardb)
#<test-run: 24 tests, 3528843 assertions, 0 failures (0 expected) in 30.306 sec>

Credits
===

By MSI <http://www.msi.co.jp>

Thanks to Pascal Costanza for MOP conformance and other advice.

This project has found bugs in all Lisp implementations tested (Allegro,
Lispworks, SBCL, and ClozureCL). Prize goes to SBCL for correctness as
the bug (slot-value didn't work) was already fixed in a newer version.
