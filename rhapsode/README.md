# Rhapsode
[Repository](https://git.argonaut-constellation.org/~alcinnz/rhapsode) | [Issue Tracker](https://todo.argonaut-constellation.org/~alcinnz/rhapsode) | [Website](https://rhapsode-web.org/)

Web browser targetting voice I/O.

See CONTRIBUTING.md for instructions on how to help the project.

## Desired Experience
Rhapsode will read a webpage out loud to you according to some special CSS properties.
Arrow key navigation will be supported for navigation between headings, paragraphs,
and table cells.

Links (defined generally enough to include `<audio>`, etc) will be indicated via
a special beep for the user to repeat one back (via microphone or keyboard) at
any time in order to navigate between pages. Rhapsode will start reading from
the `<main>` element if present.

Forms will be rendered as ordinary elements and be navigated to as a link. Once
navigated to you will be prompted to fill each input of the form in turn before
reviewing/correcting your submission before sending it.

## Why?
Because I want The Web to be "universal", for browsers to have the slack they
need to present it however best suits their users and the devices they use.
Rhapsode demonstrates HTML/CSS's capabilities this way.

And because I want to discourage the excessively bloated RCE vulnarability that
is JavaScript, which does plenty of harm even without breaking out of it's sandbox!
I want browsers to be simple and easy to audit or write your own.

See the wiki for webdev documentation.

## Installation/Running
1. Install `gstreamer1.0-pocketsphinx` `zlib1g-dev`, `ghc`, `cabal-install`. (Debian package names listed here)
2. Install the C libraries `espeak-ng`, `sndfile`, `gstreamer1.0`, & `openssl` (on Debian: `libespeak-ng-dev`, `libsndfile1-dev`, `libgstreamer1.0-dev`, & `libssl-dev`)
3. If you've installed Haskell via e.g. GhcUp, you may need to edit `src/Types.hs` so that `buildDir` is assigned to the absolute path to the Rhapsode repository being built. This is used to know where to find the datafiles.
4. (OPTIONAL) Add `~/.cabal/bin/` to your path if it isn't already.
5. Run Rhapsode with `cabal run https://example.com/`, where "https://example.com/" can/should be replaced any valid URL. Or if you followed step 3, you can replace "cabal run" with "rhapsode".

## Contributing
Please send patches to our [issue tracker](https://todo.argonaut-constellation.org/~alcinnz/rhapsode) by either:

* Attaching the output of `git format-patch`.
* [git send-email](https://git-send-email.io/) to ~alcinnz/rhapsode@todo.argonaut-constellation.org .
* Linking to your fork elsewhere.

Whichever you find most convenient.

If you'd prefer to not make your email address public please contact alcinnz@argonaut-constellation.org !
