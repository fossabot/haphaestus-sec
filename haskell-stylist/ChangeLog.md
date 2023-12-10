# Revision history for stylist

## 2.7.0.1 -- 2023-06-20
* Fix build when downloaded from Hackage (was missing a file)

## 2.7.0 -- 2023-05-16
* Improved crash resiliency in rendering numbers
* Allow prioritizing specific properties in cascade.
* Renamed `priority` field of queryable stylesheets to avoid conflict in reexports (hence major version)

## 2.6.0 -- 2023-05-04
* Improved text generation
* Added infrastructure forwarding pseudoelements to PropertyParsers
* @counter-style support, with large builtin suite.

## 2.4.0 -- 2020-12-31
* Add `@document regexp()` support via TDFA.
* Allow callers to define psuedoclasses via callbacks on attribute value.

## 0.0.1  -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
