---
layout: docs
title:  "Bound value"
section: "fileFormat2"
---

# Bound value

A bound value exists either for a lower or upper bound with respect to a particular set of correlations among which the local, quantum or nonsignaling set.

A bound value can either be:

- an exact real number from the field of cyclotomics (but not a decimal),
- an interval, whose bounds are either exact real cyclotomics or a decimal number.

## Exact real cyclotomic number

An exact cyclotomic number is written either as a single basic term, or as a sum of basic terms. In a sum, the first term can be prefixed by a minus sign (-)(, and the next terms are either added (+) or substracted. The basic term described below is signless.

A basic term is either:

- an arbitrary large integer (ex: 2342342),
- a rational fraction (ex: `2/3`, `34 / 23`),
- a square root of a nonnegative integer or rational number as described above (ex: `sqrt(2/3)`),
- the cosine of a rational multiple of pi, of the form `cos([n[/d]*]pi[/d])` where the optional parts are written in brackets, `n` is a integer or rational number optionally prefixed by `-` and `d` is a positive integer,
- the sine of a rational multiple of pi, of the form `sin([n[/d]*]pi[/d])`,
- a square root, sine or cosine, which we denote for this item by `x`, written `[n[/d]*]x[/d]`, where `n` and `d` are as above.

## Decimal number

A decimal number denotes a number only known up to some precision. It is written:

- alone as `[-][i.f`, where `i` represents an integer (either `0` alone, or a digit from `1` to `9` followed by arbitrary digits), `f` the fractional part (an arbitrary string of digits, possibly empty),
- after scaling and shifts, as`[-][i[/d]*]i.f[/d]` where `d` is a positive integer.

Decimal numbers are interpreted as exact decimal fractions, i.e. `0.4 = 2/5`, but they are kept as is during the computations. Moreover, decimal numbers can only enter interval bound values, not exact bound values.

## Interval

An interval is either:

- a bounded interval of the form `[l,u]` so that the value `x` is such that `l <= x <= u`,
- an open interval of the form `]-inf,u]`, so that the value `x` is such that `x <= u`,
- an open interval of the form `[l,inf[`, so that the value `x` is such that `l <= x`.

The lower and upper bounds `l` and `u` can be either exact real cyclotomic numbers or decimal numbers.
