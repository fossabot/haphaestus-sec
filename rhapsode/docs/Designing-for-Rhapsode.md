## 1. Write Semantic (X)HTML5
Use *changes* in Rhapsode's voice to *enhance* the communication of your text,
that'll help people pay attention to it. To do so start by making meaningful
use of the (X)HTML5 tags, and then (if you want) you can get more specific with
Rhapsode-specific CSS. At the very least provide good links.

If you've got quotes, marking them with `<q>` or `<blockquote>` tags rather than
quote marks will render much more clearly in Rhapsode. Rhapsode will then render
these in a new voice, while visual browsers will still render them as the
appropriate quote marks for your language.

## 2. Declare Your Page's Language
If Rhapsode knows what language your page is written in, it can alter some of
the phrases it inserts to match (not that it does yet). To do so use the `lang`
or `xml:lang` attributes on the root `<html>` element.

This mostly just applies if you've got forms on the page.

## 3. Don't Rely on JavaScript
Rhapsode doesn't support JavaScript, as it's APIs don't map cleanly to Rhapsode's
experience. And because Rhapsode doesn't like it's complexity or security model.

As such if your pages break when JavaScript's disabled, they'll almost certainly
break in Rhapsode. The exception is for simple scripts that alter the style of an
existing element, because Rhapsode follows an alternative set of CSS properties.

## 4. Avoid Excess Text
Listeners may tune out if you don't get straight to the point and stay on topic.
As for styles, it doesn't matter much what you do as long as you're consistant.

## Navigation
There are a couple of additional points when it comes to navigation.

**NOTE:** Navigation has not yet been implemented.

### 5. Never Override `:link {cue-after}`
Visitors will be relying on this audio cue to know this is a link they can follow,
and override it defeats the purpose of the link.

In fact, you are not permitted to do so. This still rule is `!important`.

### 6. Don't Rely on Navbar (Or Adjust Styles)
Because excess text can bore the visitor, Rhapsode defaults to not read out your
`<nav>` tag. However it'll still allow visitors to follow these links if they
can intuit that they exist, so it's still very useful to provide a navbar on
your pages.

As such you should make sure that visitors can navigate your entire site without
using the navbar. With the navbar itself acting as an enhancement but not a
necessity.

Or alternatively you can override this default via the CSS `nav {speak: always}`.
I would suggest applying this style *only* to your homepage, so it doesn't get
in the way of your site's text.

## 7. Reset All Properties In Your Voice Stylesheets
Rhapsode reserves the right to adjust it's user agent stylesheet to better suite
the majority of sites not targetting Rhapsode specifically. As such you should not
rely on these defaults staying as they are when styling your own pages for it.

The good news is that there's not that many properties to reset to `initial`.

---

[elementary OS's blog](https://blog.elementary.io/) sounds great in Rhapsode, for example.
