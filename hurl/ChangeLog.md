# Revision history for hurl

## 2.3.0.0 -- 2022-10-31
* Fix MIMEtypes on error messages.
* Fix overly-strict certificate validation, sacrificing clientside certificates.

## 2.2.0.0 -- 2022-08-06
* Fix webform submission, refine API, & support multiple encodings.
* Switch from OpenSSL to `tls`/Cryptonite for a cryptographic backend for better error reporting & to fix Gemini implementation
* Support clientside certificates in Gemini & HTTPS
* Support HSTS with bypass
* Allow overriding HURL's error-reporting localization

## 2.1.1.0 -- 2021-07-22
* Add support for submitting forms (fallback to normal URL resolution).
* Allow setting cookies in response to HTTP POST, including retroactively for the sake of CSRF protections.

## 2.1.0.1 -- 2021-03-09
* Fixes a build failure.

## 2.1.0.0
* Added APIs for localizing MIMEtypes
* Crash fixes

## 2.0.0.0 -- 2021-01-07
* Fix several real & potential crashes
* Expose APIs for querying localized labels for MIME types from the OS
* Expand search path for executable extensions
* Subsume Appstream feature flag into FreeDesktop, XML dependency is now nearly-required on Linux.

## 1.5.0.0 -- 2020-12-24
* Add HTTP caching
* Add `ext:` URI scheme as a plugin system.
* Expose APIs list all apps for a given MIMEtype & open a URL in one.

## 0.1.0.0  -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
