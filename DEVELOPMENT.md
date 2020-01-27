# Development notes and tips

I build and test assumpta and assumpta-core
using [`stack`](https://docs.haskellstack.org/en/stable/README/),
plus CI services (Travis CI and CircleCI).

CircleCI in general is much more irritating, doesn't natively support
'test matrixes', and has obviously had less thought put into its design;
but it's faster, seems to do a better job of cacheing build artifacts,
and lets you use any Docker image you like to run tests in (unlike Travis CI,
which only has a set number of support environments).

The doctests currently require stack to run, and will
fail if a `STACK_RESOLVER` environment variable isn't found (or if
they can't find `stack` on the `$PATH`.

So they are best run like this:

```bash
$ export STACK_RESOLVER=lts-12.26
$ stack --resolver="${STACK_RESOLVER}" test --flag assumpta-core:test-doctests
```

If anyone wants to suggest things which might make the source tree
more friendly to cabal-users, feel free to let me know.

## SMTP Standards

The original SMTP standard was
[RFC 821](https://tools.ietf.org/html/rfc821), defined in 1982.

Most MTAs today use
[RFC 5321](https://tools.ietf.org/html/rfc5321), released in 2008,
which extends SMTP to (inter alia) add security and some new commands.
There are also errata to RFC 5321, available at
<https://www.rfc-editor.org/errata/rfc5321>.

I hope to mostly implement RFC 5321. The various forms of
authentication are detailed in other standards, which I
still need to track down.

## Doctests

The test suite `doctest-test` currently (AFAIK) requires
stack to run -- I tried it with ghc 8.0.2, cabal-install
2.4.1.0, and version 0.16.2 of the doctest library, and
got a segfault.

But since the aim of the test suite is to test the
documentation -- not doctest, nor cabal, nor ghc -- 
I didn't spend time investigating the problem.

The test runner doesn't itself actually call `stack` -- but
if run by `stack test`, it gets passed exactly the
environment it needs.



