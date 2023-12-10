# Haskell URL
[Repository](https://git.argonaut-constellation.org/~alcinnz/hurl) | [Issue Tracker](https://todo.argonaut-constellation.org/~alcinnz/hurl) | [Hackage](https://hackage.haskell.org/package/hurl)

HURL resolves URLs to the `Text` or `ByteString` data they reference, alongside
it's MIMEtype. It is capable of resolving data to the charset specified by the
MIMEtype, and will convert any errors to response data.

It's main function is `Data.Network.Fetch.fetchURL`, which takes a "Session"
initialized by `Data.Network.Fetch.newSession`.

## Supported URI schemes
HURL can resolve the following URL schemes:
* `http(s):` - via [http-client](https://hackage.haskell.org/package/http-client)
* [`gemini:`](https://gopher.tildeverse.org/zaibatsu.circumlunar.space/1/~solderpunk/gemini) - being developed by [@solderpunk@tilde.zone](https://tilde.zone/@solderpunk)
* `file:`
* `data:` - via [bytestring-base64](https://hackage.haskell.org/package/bytestring-base64)
* Native apps on [FreeDesktop.Org](https://www.freedesktop.org/wiki/Specifications/)-compliant systems (including basically any non-Android Linux system)
* Recommends apps to install on systems which provide a [AppStream Database](https://www.freedesktop.org/wiki/Distributions/AppStream/).

Each of these protocols can individually be turned on or off at compiletime.

## Desired URI schemes
Integration of the following URL schemes would be appreciated:

* `ftp(s):` - Outdated Haskell implementations exists.
* `magnet:` - Outdated/incomplete Haskell implementation exists.
* `gopher:` - Would want to pair with a [Markdown-to-HTML](https://hackage.haskell.org/package/pandoc) parser to restore semantics lost in Rhapsode's conversion to audio.
* `xdg-icon:` - Custom, looks up images according to [icon-theme-spec](https://www.freedesktop.org/wiki/Specifications/icon-theme-spec/)
* [`idsc:`/`mdsc:`](https://datashards.net/) - being developed by [@cwebber@octodon.social](https://octodon.social/@cwebber).
* Dispatch to apps on Mac OS X, Windows, Android, and/or iOS.

Feel free to suggest more, point me towards the function(s) to call, or write the
patch yourself! Please email these to `opensource@openwork.nz`.

## Contributing
Please send patches to our [issue tracker](https://todo.argonaut-constellation.org/~alcinnz/hurl) by either:

* Attaching the output of `git format-patch`.
* [git send-email](https://git-send-email.io/) to ~alcinnz/hurl@todo.argonaut-constellation.org .
* Linking to your fork elsewhere.

Whichever you find most convenient.

If you'd prefer to not make your email address public please contact alcinnz@argonaut-constellation.org !

