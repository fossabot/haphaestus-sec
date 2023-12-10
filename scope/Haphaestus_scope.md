# Haphaestus audit proposal

## Introduction

[Haphaestus][haphaestus] is an open source browser to be used on TVs through a remote developed by [Argonaut Constellation][argonaut] and financed by NLnet. Haphaestus has the peculiarity of being javascript free. It is developed using Haskell on top of the Argonaut stack, a collection of open source modules handling various aspects of the product, from a CSS renderer to a URL resolver. The [NLnet][nlnet] funding includes a security audit by [Radically Open Security (ROS)][ros], a non-profit company centered around software security. 

## Disclaimer

It is important to note that the Haphaestus product is currently under development, and no version of the browser itself is available to be audited. Instead, only the stack that Haphaestus is built on can be assessed in addition to [Rhapsode][rhapsode], another browser from the Argonaut Constellation on which Haphaestus will be based as well.

This means that any findings surfaced by this audit might no longer be relevant in the final product, and items that were deemed safe could eventually become problematic. In addition, a security audit in itself bears no guarantee of ensuring that the product is foolproof to attacks or any kind of vulnerability. Instead, it is used to decrease the likelihood of such vulnerabilities existing and being exploited by attackers.

## Stack and core components

A challenge of this audit is to link code elements to security properties while the final integration is being completed. The major components to consider are part of the [Argonaut stack][argonaut]. They can be found in two places: the [SourceHut][jaro] page of the first initial contributor, and the [SourceHut][alcinnz] page of the current main contributor, [Adrian Cochrane][adrian]. They are as follows:

- [fontconfig-pure][fontconfig] is a font rendering engine containing bindings to fontconfig.
- [CatTrap][cattrap] contains partial integration tests. It allows for downloading the page, styling and rendering it.
- [hardfuzz-pure][harfbuzz] is a text shaping library.
- [bureaucromancy][bureau] is a library to parse and render forms.
- [haskell-stylist][stylist] is a css handling engine.
- [hurl][hurl] is the direct interface to the network.

## Scope

### Scope overview

We propose a two fold audit, consisting of:

1. A high-level security review of the project’s security model.
2. A code review to manually assess the correctness of the code with respect to the high-level security properties below.

### Security model

Here are the key elements of the security model for Haphaestus:

1. **Javascript-free**: Haphaestus is javascript free.
1. **Minimal-user-info**: Web servers should receive limited information by default (pages requested by users, desired language, browser behaviour, and two types of bandwidth reduction data – bucketed screen size and etags).
3. **Media-requests-allowed**: While a page is being downloaded, additional requests can be sent, such as images or other media parts of the page.
4. **Interaction-data-minimized**: After a page has been downloaded, no requests that leak interaction data (text you've selected, keypresses, zooming, anything finer grained then clicking links or submitting forms) shall be sent. Pages with dynamic content such as news feed, should require user input to allow the updates.
5. **Sender-identity-protected**: By default, servers should not be able to match requests coming from the same person, except by using browser sniffing which is not disallowed.
6. **Turing-completeness-forbidden**: Haphaestus should not run any Turing-Complete programs provided by web servers, aside from those embedded in fonts.
7. **Crash-free**: Web pages should never be able to crash the browser. This means that the exception handling mechanism should be robust and complete, and that there should not be infinite loops.
8. **Robust-memory-model**: Memory handling system should be robust.
9. **Private-keys-safety**: Private keys used for passkey authentication should not be leaked.

### High level security review

A security expert will be provided to assess the completeness and relevance of the security model of Haphaestus, based on the previous items. The involvement of this expert will be small considering the current development state of the product.

### Code review

A manual code review will be conducted by a safety/Haskell expert. This code review will attempt to surface discrepancies between the security model of Haphaestus and its implementation. Note that, based on the partial nature of the project, some of these properties might be ignored, when the code to support them is absent. In addition, this code review might surface other bugs and issues that are unrelated to the model. Code good practices will also be advised when applicable.

In particular, here is a rough mapping between security properties and components:

|#|Name|Components|
|:---:|:---:|:---:|
|1|Javascript-free|[rhapsode][rhapsode], [CatTrap][cattrap]|
|2|Minimal-user-info|[hurl][hurl], [stylist][stylist]|
|3|Media-requests-allowed|[CatTrap][cattrap], [stylist][stylist]|
|4|Interaction-data-minimized|[CatTrap][cattrap], [stylist][stylist]|
|5|Sender-identity-protected|[hurl][hurl]|
|6|Turing-completeness-forbidden|[CatTrap][cattrap], [stylist][stylist]|
|7|Crash-free|all components|
|8|Robust-memory-model|[font-config][fontconfig], [harfbuzz][harfbuzz]|
|9|Private-keys-safety|**not implemented yet**|

## Duration of work and starting date

We propose to begin this work on **Monday the 27th of November** for the following overlapping durations:
- Security review: 3 days
- Code review: 10 days
This includes the reporting time and so the final report should be delivered on **Friday the 8th of December**. Any delay would not yield additional cost.

## References

[aconstel]: https://argonaut-constellation.org/
[haphaestus]: https://haphaestus.org/
[jaro]: https://git.argonaut-constellation.org/~jaro/
[balkon]: https://git.argonaut-constellation.org/~jaro/balkon
[alcinnz]: https://git.argonaut-constellation.org/~alcinnz/
[fontconfig]: https://git.argonaut-constellation.org/~alcinnz/fontconfig-pure
[harfbuzz]: https://git.argonaut-constellation.org/~alcinnz/harfbuzz-pure
[rhapsode]: https://git.argonaut-constellation.org/~alcinnz/rhapsode
[bureaucromancy]: https://git.argonaut-constellation.org/~alcinnz/bureaucromancy
[stylist]: https://git.argonaut-constellation.org/~alcinnz/haskell-stylist
[cattrap]: https://git.argonaut-constellation.org/~alcinnz/CatTrap
[hurl]: https://git.argonaut-constellation.org/~alcinnz/hurl
[nlnet]: https://nlnet.nl/
[ros]: https://www.radicallyopensecurity.com/
[adrian]: https://adrian.geek.nz/
