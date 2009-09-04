manardb is a portable (across Lisps on Linux) memory mapped database for Common Lisp.
===

It frees one from the garbage collector but stays within MOP.

By MSI <http://www.msi.co.jp>

This project has found bugs in all Lisp implementations tested (Allegro,
Lispworks, SBCL, and ClozureCL). Prize goes to SBCL for correctness as
the bug (slot-value didn't work) was already fixed in a newer version.
