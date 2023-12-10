# Links

You should be able to follow links by repeating back it's text either via speech or typing.

---

To expand upon this design, if only a single link matches the browser would immediately start loading that page and announce that it's doing so.
But if multiple links match it'll list each link's text, title, and href alongside a shorthand letter with which to refer to it in your response.

---

These links would be defined very generally, and include:

* "psuedolinks" inserted by the browser to take the place of toolbar buttons.
* "petnames" used to quickly navigate to prespecified sites.
* bookmark "tags" used to search your bookmarks. Or should I just rely on petnames here?

These different types of links would be styled differently during the clarification stage so users are clear what's a part of the browser and what's part of the web. This distinction may be necessary for security, and I'll struggle to maintain it otherwise.

---

- [x] Extract links from page
- [x] Switch to C for access to needed libraries
- [x] Filter links based on user input
- [ ] Integrate voice recognition (CMU Pocket Sphinx or Mozilla Deep Speech)
- [x] Implement "petnames" configured via an XBEL-derived format.

## Petnames
Petnames would be a core part of Rhapsode's "chrome", as this would serve as a
bookmarks bar that can fit as many as many links as you can remember. It would
also be the means by which users can access any navigation controls packaged up
with Rhapsode.

It would even give a great experience for unmodified (JS-free) search engines,
which you could bring up by name and immediately be prompted for a query.
