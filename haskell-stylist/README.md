# Haskell Stylist
[Repository](https://git.argonaut-constellation.org/~alcinnz/haskell-stylist) | [Issue Tracker](https://todo.argonaut-constellation.org/~alcinnz/haskell-stylist) | [Hackage](https://hackage.haskell.org/package/stylist)

Generic CSS style engine for Haskell, intended to aid the development of new browser engines.

Haskell Stylist implements CSS selection and cascade (but not inheritance) independant of the CSS at-rules and properties understood by the caller. It is intended to ease the development of new browser engines, independant of their output targets.

For more interesting projects see: https://argonaut-constellation.org/

## Versioning
The second major number indicates that more of CSS has been implemented within the existing API. Until then the error recovery rules will ensure as yet invalid CSS won't have any effect.

The first major number indicates any other change to the API, and might break your code.

## API
To parse a CSS stylesheet call `Data.CSS.Syntax.StyleSheet.parse` which returns a variant of the passed in `StyleSheet`. `StyleSheet` is a typeclass specifying methods for parsing at-rules (`parseAtRule`), storing parsed style rules (`addRule`), and optionally setting the stylesheet's priority (`setPriority`).

If these ultimately call down into a `Data.CSS.Syntax.Style.QueryableStyleSheet` you can call `cascade` to resolve them into any instance of `PropertyParser`. Or you can use `queryRules`/`cascade'` to handle the pseudoelements yourself before applying the cascade.

`PropertyParser` allows to declaratively (via Haskell pattern matching) specify how to parse CSS properties, and how they're impacted by CSS inheritance. It has four methods: `longhand` and `shorthand` specify how to parse CSS properties, whilst `temp` and `inherit` specifies what the default values should be.

### Building
1. Install `ghc` and `cabal-install`. (Debian package names listed here)
2. From within the git repository, run `cabal install`. This'll compile Stylist and all it's other dependencies.
3. Run `cabal test` after every change you make.

## Contributing
Please send patches to our [issue tracker](https://todo.argonaut-constellation.org/~alcinnz/haskell-stylist) by either:

* Attaching the output of `git format-patch`.
* [git send-email](https://git-send-email.io/) to ~alcinnz/haskell-stylist@todo.argonaut-constellation.org .
* Linking to your fork elsewhere.

Whichever you find most convenient.

If you'd prefer to not make your email address public please contact alcinnz@argonaut-constellation.org !
