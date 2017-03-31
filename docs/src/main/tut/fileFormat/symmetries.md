---
layout: docs
title:  "Bell expression symmetries"
section: "fileFormat"
---

# Bell expression symmetries

Information about the symmetry group of an expression can be optionally written down in the YAML file under the optional `symmetries` key. The Faacets command line tool can also compute this information from scratch.

When `symmetries` is provided, the following properties are required:

- numberOfRepresentatives
    An integer giving the number of representative of the Bell expression under relabelings. The order of the symmetry group of the Bell expression can be then computed using Lemma 1 of [arXiv](http://www.arxiv.org/).

- remarkableGenerators

    This section list a set of generators for the symmetry group of the Bell expression. Generators are grouped according to the remarkable subgroups they are part of, according to the following sequence of subgroups:

       - _liftings_: relabelings involving outcomes of a single setting of a single party
       - _outputPermsPerParty_: relabelings involving outcomes of a single party
       - _outputPerms_: general outcomes relabelings
       - _inputPermsPerParty_: relabelings involving settings of a single party
       - _outputInputPermsPerParty_: relabelings involving settings and outcomes of a single party
       - _outputInputPerms_: relabelings involving settings and outcomes
       - _partyPerms_: relabelings involving parties only
       - _rest_: additional generators

As an example, here is the symmetry information for the CHSH expression.

``` yaml
symmetries:
  numberOfRepresentatives: 8
  remarkableGenerators:
    outputPerms: ['A1(1,2) A2(1,2) B1(1,2) B2(1,2)']
    outputInputPerms: ['A2(1,2) B(1,2)', 'B1(1,2) A(1,2)']
    rest: ['A(1,2) B(1,2) (A,B)']
```

## Todo

Add link to our paper, to the Faacets command line tool documentation
