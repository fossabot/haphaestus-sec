# FontConfig Pure
[Source Code](https://git.argonaut-constellation.org/~alcinnz/fontconfig-pure) | [Issues](https://todo.argonaut-constellation.org/~alcinnz/fontconfig-pure) | [Hackage](https://hackage.haskell.org/package/fontconfig-pure)

Pure-functional language bindings to [FontConfig](https://www.freedesktop.org/wiki/Software/fontconfig/). Mostly consists of conversions between common Haskell types & FontConfig's imperative C types.

Queries a database of fonts, both in-memory & on the local filesystem, to find the one most closely matching your desired font-family, style, & size. Includes bridges from [Haskell Stylist](https://hackage.haskell.org/package/stylist) and to both FreeType & (soon) Harfbuzz.

## Requirements
* FontConfig 2.13 or greater
* LibStdC++ (for Text dependency)
* Cabal

Cabal can install the rest.

## Use
Please see the Haddock documentation for API usage.

Contains a demo program in `Main.hs`, which can be run via `cabal run`, to query the system font database using standard FontConfig query syntax given in commandline arguments.

There's a tentative expansion with its own testscript which bridges over to Harfbuzz-Pure, & includes a testscript which renders "sphinx of black quartz judge my vow" using the best matching fontfile for your query. This is being held up until a new version of Haskell CSS Syntax is released to Hackage supporting UTF-8 Text.

## Contributing
Please send patches to our [issue tracker](https://todo.argonaut-constellation.org/~alcinnz/fontconfig-pure) by either:

* Attaching the output of `git format-patch`.
* [git send-email](https://git-send-email.io/) to ~alcinnz/fontconfig-pure@todo.argonaut-constellation.org .
* Linking to your fork elsewhere.

Whichever you find most convenient.

If you'd prefer to not make your email address public please contact alcinnz@argonaut-constellation.org !
