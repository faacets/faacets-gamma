---
layout: docs
title:  "Lower and upper bounds"
section: "fileFormat"
---

# Lower and upper bounds

Oriented Bell expressions are described by a Bell expression along with a direction `<=` or `>=`. Data about an oriented Bell expression is written using a BellExpression along with additional data in the special properties lower or upper, describing knowledge about the `expr >= bound` and `expr <= bound` directions respectively.

Each direction `lower` and `upper` can have the following properties:

## bounds

Bounds corresponding to different sets of interest can be listed here through a mapping list. The mapping keys `local`, `quantum` and `no-signaling` are reserved for the usual accordingly named sets. The mapping values are given by a string expression made of:

 -       integers,
 -       infinity written `inf`,
 -       rational numbers written down using the format `numerator/denominator`,
 -       decimal numbers written using `digits.digits`,
 -       arithmetic operators `+`, `-`, `*`, `/`,
 -       parenthesis,
 -       intervals written down as `[lb, ub]`, where `lb`, `ub` are expressions.

    Here is an example of section specifying several bounds:

``` yaml
bounds:
  local: 2
  quantum: [-inf, 2.828427124746191]
  nosignaling: 4
```

Here, the `[-inf, 2.828427124746191]` interval expression for the quantum bound interval signifies that we have obtained an approximation for the quantum bound through relaxations, so that the real upper bound is below `2.828427124746191`. The `-inf` side of the interval could be replaced by finding an explicit state and measurements that achieve a quantum value.

## keywords

Keywords can also be associated to an oriented Bell expression. For now, no keywords are reserved, nor is any keywords automatically computed for oriented expressions. Keywords here respect the same syntax as the ones for Bell expressions mentioned above.

Special keywords specify if a bound corresponds to a facet of some polytope. The keywords `facet-polytope` and `not-facet-polytope` can be used for that effect, where `polytope` corresponds to the name of a bound. The keywords `(not-)facet-local` and `(not-)facet-no-signaling` are reserved. If the facet-defining property is not known, the corresponding keyword is not present.

    The following is a valid `keywords` section:

``` yaml
keywords: [facet-local]
```

Two orientations of a Bell expression can be specified in a single file by incorporating both a `lower` and an `upper` section in the file. This can provide a full decription of the bounds satisfied by a given Bell expression.
