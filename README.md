# Disassembly of "Double-Duty DOS"

"Double-Duty DOS" by Jason Coleman originally appeared in Compute! Magazine, Issue 89 (October 1987), as a type-in binary listing. It extends the Apple II ProDOS-8 operating system's BASIC.SYSTEM interpreter with three additional commands to enable interop with the older DOS 3.3 for the Apple II. See the [original article](./Article.md) for more details.

## Observations

* `DLOAD` always loads whole 256-byte sectors, regardless of the file length. This means that e.g. loading a 128-byte binary file to $300 would trash the vectors at $3Fx.
* Both `DLOAD` and `DSAVE` for a binary file trash the 4 bytes before the load/save address.
* Despite the documentation in the article, `DSAVE` should support address (A) and length (L) parameters specified in decimal, not just hex, since BASIC.SYSTEM is doing the parsing.
* The `DSAVE` logic handles overwriting an existing file (of the same type) by deleting it first; this could easily be exposed as a `DDELETE` command.
* The `DSAVE` command trashes $800.
