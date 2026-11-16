# EmacsExpert

A deterministic, model-free Emacs documentation expert owned by dotfiles.

The build runs a clean `emacs -Q --batch` process, exports inert documentation
records, compiles them into Prolog facts, and exposes bounded read-only queries.
Variable values and private user init are not exported.

This is the bounded core-loaded corpus slice moved from `lost-rob0t/zara-plugins#858`.
It does not claim complete Emacs/package coverage; the generated manifest records
the missing coverage classes explicitly.

Runtime registration will consume the canonical ZARA-EXPERT/1 adapter once that
contract lands. This package deliberately does not invent a parallel activation
format.
