# Pure-functional Harfbuzz language bindings.
[Git repo](https://git.argonaut-constellation.org/~alcinnz/harfbuzz-pure) [Issue Tracker](https://todo.argonaut-constellation.org/~alcinnz/harfbuzz-pure) [Hackage](https://hackage.haskell.org/package/harfbuzz-pure)

HarfBuzz is a text shaping library. Using the HarfBuzz library allows programs to convert a sequence of Unicode input into properly formatted and positioned glyph output for practically any writing system and written language.

NOTE: You may need to install Harfbuzz 3.3.0 (Jan 2022) or newer from source, it hasn't been widely packaged yet.

These APIs map closely to those Harfbuzz 3.3.0 exposed, but tweaked to be pure-functional & reuse common Haskell datatypes. They throw HarfbuzzError should they run out of memory, though that should be extremely rare & otherwise contain no side-effects.

## Requirements

* Harfbuzz 3.3.0 or greater
* LibStdC++ (for Text dependency)
* Cabal

Cabal is capable of installing the few other dependencies.

For normal use please install via Hackage.

## Use
Please see the Haddock documentation for API usage.

Contains a demo program in `Main.hs`, which can be run via `cabal run`, that textually outputs the shaping data for given text under the [Lora](https://fonts.adobe.com/fonts/lora) font. Commandline arguments, if any, are the text to be rendered. Otherwise uses "Testing, testing".

## Contributing
Please send patches to our [issue tracker](https://todo.argonaut-constellation.org/~alcinnz/harfbuzz-pure) by either:

* Attaching the output of `git format-patch`.
* [git send-email](https://git-send-email.io/) to ~alcinnz/harfbuzz-pure@todo.argonaut-constellation.org .
* Linking to your fork elsewhere.

Whichever you find most convenient.

If you'd prefer to not make your email address public please contact alcinnz@argonaut-constellation.org !
