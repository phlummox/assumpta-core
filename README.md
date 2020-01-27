# assumpta-core

# assumpta-ci [![Hackage version](https://img.shields.io/hackage/v/assumpta-ci.svg?label=Hackage)](https://hackage.haskell.org/package/assumpta-ci) [![Linux Build Status](https://img.shields.io/travis/com/phlummox/assumpta-core.svg?label=Linux%20build)](https://travis-ci.com/phlummox/assumpta-core) [![phlummox](https://circleci.com/gh/phlummox/assumpta-core.svg?style=svg)](https://circleci.com/gh/phlummox/assumpta-core)

A library for constructing SMTP clients, which provides the core functionality
for [assumpta](https://hackage.haskell.org/package/assumpta).
It provides a monad transformer and an mtl-style class for sending SMTP
commands (including `STARTTLS`) to a server and checking for expected
responses, over some abstract connection type.

It does not provide a concrete implementation
which can actually be used to communicate over
a network - for that, see the
[assumpta](https://hackage.haskell.org/package/assumpta) package.

## Installation

`assumpta-core` can be installed in the standard way using `stack`
or `cabal` (e.g. `stack install assumpta-core`).

## Usage

See the [assumpta](https://hackage.haskell.org/package/assumpta) package
for examples of usage.

Typically, you will want to:

-   write an instance of the `Connection` class in
    [Network.Mail.Assumpta.Connection](http://hackage.haskell.org/package/assumpta/docs/Network-Mail-Assumpta-Connection.html),
    providing a concrete implementation in terms of some networking
    library.
    ([assumpta](https://hackage.haskell.org/package/assumpta) contains
    one based on 
    [connection](https://hackage.haskell.org/package/connection).)
-   for convenience, write a type synonym for the `SmtpT` transformer,
    specialized over your new `Connection` instance
    (`MySmtpT = SmtpT MyConnection`).
-   And that should be enough for you to start communicating
    with a server. See the examples in
    [assumpta](https://hackage.haskell.org/package/assumpta) for more
    details.

## FAQ

-   Q. Why the name 'Assumpta'?

    A. Dunno, I just like it as a name. It means "assumption" you know.
    I find I make many of those when programming.    

## Acknowledgements

Many thanks to 
[Alexander Vieth](https://github.com/avieth)
(author of [smtp-mail-ng](http://hackage.haskell.org/package/smtp-mail-ng)),
and to [Jason Hickner](https://github.com/jhickner) and
[Matt Parsons](https://github.com/parsonsmatt)
(authors of [smtp-mail](http://hackage.haskell.org/package/smtp-mail)),
whose packages provided the original basis for the
assumpta-core code.
