---
layout: docs
title:  "Minimal keys for Bell Expressions"
section: "fileFormat"
---

# Minimal keys for Bell Expressions

The simplest Bell expression is described by the following YAML fragment:

```yaml
type: BellExpression
scenario: '[(2 2) (2 2)]'
representation: Non-signaling Correlators
coefficients: [0, 0, 0, 0, 1, -1, 0, 1, 1]
```

The following properties are always required:

- type: the type of a Bell expression is always a string equal to `BellExpression`.
- scenario: the String describing the scenario in which the expression is defined. The format of scenarios is detailed in Appendix A of [arXiv](http://www.arxiv.org/).

For example, a scenario with two parties, two settings and two outcomes is specified by:

```yaml
scenario: [(2 2) (2 2)]
```

- representation: This key specifies the parametrization used for the Bell expression coefficients. The supported representations are detailed in [Representations](http://docs.faacets.com/latest/concepts/representation.html).
- coefficients: Vector of integer or rational coefficients describing the Bell expression. In the case of integer coefficients, the value associated to the `coefficients` key is a YAML sequence of integers. In the case of rational coefficients, the value associated to the `coefficients` key is a mapping with `numerator` and `denominator` keys. The value associated to the `numerator` key is a sequence of integer, and `denominator` is a single integer acting as the common denominator of the coefficients.

Here are several examples of valid Bell expressions.

The CHSH inequality, written in the Collins-Gisin notation:

```yaml
type: BellExpression
scenario: '[(2 2) (2 2)]'
representation: Non-signaling Collins-Gisin
coefficients: [0, -1, 0, -1, 1, 1, 0, 1, -1]
```

The Guess Your Neighbor’s Input inequality, written using full Probabilities:

```yaml
type: BellExpression
scenario: '[(2 2) (2 2) (2 2)]'
representation: Signaling Probabilities
coefficients:
    numerator: [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    denominator: 4
```
