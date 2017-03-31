---
layout: docs
title:  "Keywords"
section: "fileFormat"
---

# Keywords

This section allows for the specification of properties satisfied by a Bell expression. Keywords can consist of alphanumerical characters plus the `-` and underscore `_`. They are specified as a sequence of strings:

``` yaml
keywords: ['minimal', 'not-io-lifted']
```

The following keywords are reserved, and can be computed automatically using the `Faacets` command-line tool:

- minimal
    The Bell expression is the minimal lexicographic representative of its equivalence class under relabelings.
- not-minimal
    The Bell expression is known not to be minimal.
- io-lifted
    The Bell expression is known to be a lifting of settings and/or outcomes.
- not-io-lifted
    The Bell expression is known not to be a lifting of settings and/or outcomes.
- composite
    The Bell expression is known to be a composition of simpler Bell expressions, in the sense of the Section of our paper.
- not-composite
    The Bell expression is known not to be composite.
- self-opposite
    The Bell expression is equivalent under relabelings to its negative value.
- canonical
    The Bell expression is known to be `minimal`, `not-io-lifted`, `not-composite`. In addition, the scenario itself is ordered canonically, and the bound has been extracted from the coefficients, who themselves are written down using integers with GCD = 1.

## Todo

Add link to our paper, to the Faacets command line tool documentation
