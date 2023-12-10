# CSS Text-preprocessing Properties
There should be a reusable `PropertyParser` that resolves content-generation CSS
properties, so that callers only need to deal with plain text.

* [x]  content:
* [ ]  text-transform:
* [ ]  unicode-bidi:
* [x]  white-space:
* [x]  counter-reset:
* [x]  counter-increment:
* [x]  counter-set:
* [x]  content: counter()
* [x]  content: counters()
* [ ]  @counter-style
* Lists?
* Others?

It occurs to me that maybe I should upstream Rhapsode's counters implementation
to get this started.
