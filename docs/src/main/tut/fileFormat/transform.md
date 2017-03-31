---
layout: docs
title:  "Transforms"
section: "fileFormat"
---

# Transforms

## AffineTransform

This describes the transformation of a Bell expression by first multiplying its coefficients by a factor, and then adding a normalization constant. The AffineTransform object has the `of` key describing the transformed expression, and an `affine` key containing a string of the form `factor x +/- constant`. The factor has to be positive.

Example (taken from Sliwa’s second inequality):

``` yaml
decomposition:
    type: RepresentationTransform
    representation: Non-signaling Correlators
    of:
        type: AffineTransform
        affine: x + 2
        of: {type: CanonicalExpression, index: 6}
```

Example (taken from Sliwa’s tenth inequality):

``` yaml
decomposition:
    type: RepresentationTransform
    representation: Non-signaling Correlators
    of:
        type: PermutationTransform
        permutation: B2(1,2) C2(1,2) (B,C)
        of:
            type: AffineTransform
            affine: 1/2 * x + 4
            of:
                type: OppositeTransform
                of:
                    type: PermutationTransform
                    permutation: A2(1,2) B2(1,2) C1(1,2) C2(1,2) (A,B,C)
                    of: {type: CanonicalExpression, index: 13}
```

## BellExpressionProduct

A `BellExpressionProduct` describes a composition of Bell expressions. It contains a single key `of`, composed of a sequence of decomposed Bell expressions. The composition is done by attributing the first l
parties to the first component of the product, the next m parties to the second component, the next n parties to the third component and so on.

An example of the decomposition of the positivity in the scenario `[(2) (2)]`:

``` yaml
type: BellExpressionProduct
of:
    - type: AffineTransform
      affine: x + 1
      of:
          type: BellExpression
          scenario: '[(2)]'
          representation: Non-signaling Probabilities
          coefficients: [-1, 1]
    - type: AffineTransform
      affine: x - 1
      of:
          type: BellExpression
          scenario: '[(2)]'
          representation: Non-signaling Probabilities
          coefficients: [-1, 1]
```

## LiftingTransform

A `LiftingTransform` describes the addition of measurement settings/inputs or measurement outcomes/outputs, as described in [LiftingPironio](http://dx.doi.org/10.1063/1.1928727) and [FaacetsPaper](http://www.arxiv.org/). The `LiftingTransform` contains an `of` key describing the expression to be transformed, and a `lifting` key containing a string representing the lifting.

Additional outcomes are always grouped with the outcome they are lifting, and additional settings are always added at the end of measurement settings list.

The syntax for lifting strings is similar to the one for scenarios (see [Scenarios and parties](http://docs.faacets.com/latest/concepts/scenario.html)). Additional settings/inputs are described by a `+(n1 n2 ...)` component after the party description, where `n1`, `n2`, ... are the number of outcomes for each additional measurement setting.

Example: the CHSH inequality lifted in the scenario `[(2 2 2) (2 2 2)]`

``` yaml
type: LiftingTransform
lifting: [(2 2)+(2) (2 2)+(2)]
of:
    type: BellExpression
    scenario: '[(2 2) (2 2)]'
    representation: Non-signaling Probabilities
    coefficients: [-1, 1, -1, 1, 1, -1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1]
```

Additional outcomes are described by a `+(m1 m2 ... mn)` right after a measurement setting description, and``n`` is the number of measurement outcomes. The numbers mj prescribe the number of additional outcomes attached to each original measurement outcome.

Example: the CHSH inequality lifted in the scenario [(3 3) (3 3)]`

``` yaml
type: LiftingTransform
lifting: Lifting([(2+(0 1) 2+(1 0)) (2+(1 0) 2+(1 0))])
of:
    type: BellExpression
    scenario: '[(2 2) (2 2)]'
    representation: Non-signaling Probabilities
    coefficients: [-1, 1, -1, 1, 1, -1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1]
```

## OppositeTransform

An `OppositeTransform` computes the opposite Bell Expression by multiplying its coefficients by −1

. Accordingly, the information about lower bounds becomes information about upper bounds, and vice-versa. This transform has a single key `of` describing the Bell expression to transform.

## PermutationTransform

A `PermutationTransform` applies a permutation/relabeling of parties, measurement settings and/or outcomes to a Bell expression, without changing the shape of the Bell scenario. The `PermutationTransform` contains an `of` key describing the expression to be transformed, and a permutation key containing a string representing the permutation. The notation for permutations is described in [Relabelings/permutations](http://docs.faacets.com/latest/concepts/relabelings.html).

Example: decomposition of the original I3322 inequality

``` yaml
type: RepresentationTransform
representation: Non-signaling Collins-Gisin
of:
    type: PermutationTransform
    permutation: A1(1,2) A2(1,2) A3(1,2)
    of:
        type: AffineTransform
        affine: 1/12 * x - 1
        of: {type: CanonicalExpression, index: 4}
```

## RedundantTransform

A `RedundantTransform` is used to add terms acting on the signaling space to a Bell Expression. As such, it can only be applied to expression in the `SP`, `SC` or `SG` representations (see [Representations](http://docs.faacets.com/latest/concepts/representation.html)).

The transformation contains an `of` key describing the expression to transform, and a `coefficients` key containing the coefficients to add to the original expression. These coefficients have to correspond to the null vector after projection in the no-signaling space. The format of the `coefficients` key is the same as for expressions, as described in [Minimal keys for Bell Expressions](http://docs.faacets.com/latest/spec/minimal.html).

Example: the decomposition of the original Guess Your Neighbor’s Input inequality

``` yaml
type: RedundantTransform
coefficients:
    numerator: [5, 1, -1, -1, 1, -1, -1, 1, -1, 1, -1, 1, -1, -1, -1, -1, 1, -1, 1,
      -1, -1, -1, -1, -1, -1, -1, 1, 5, 1, -1, -1, 1, -1, -1, -1, -1, 1, -1, 1, -1,
      -1, 1, 1, -1, 1, 5, -1, -1, -1, 1, 1, -1, -1, -1, 5, 1, -1, -1, -1, -1, -1,
      1, -1, 1]
    denominator: 32
of:
    type: RepresentationTransform
    representation: Signaling Probabilities
    of:
        type: PermutationTransform
        permutation: B2(1,2) C1(1,2) C2(1,2) (A,B,C)
        of:
            type: AffineTransform
            affine: 1/64 * x + 1/8
            of: {type: CanonicalExpression, index: 13}
```

## ReorderingTransform

A `ReorderingTransform` is similar to a `PermutationTransform`, except that it specifically changes the shape of the Bell scenario, and is composed only of party and inputs permutations. The notation is similar to the notation of permutations, and is described under [Relabelings/permutations](http://docs.faacets.com/latest/concepts/relabelings.html).

Example: the Pironio inequality lifted in scenario `[(3 2 2) (3 2)]`

``` yaml
type: LiftingTransform
lifting: [(2+(1 0) 2 2) (3 2)]
of:
  type: ReorderingTransform
  reordering: (A,B)
  of:
    type: PermutationTransform
    permutation: A2(1,2)
    of:
      type: AffineTransform
      affine: 3/52 * x - 1/26
      of:
        type: BellExpression
        scenario: '[(3 2) (2 2 2)]'
        representation: Non-signaling Probabilities
        coefficients: [-7, 5, 5, -3, 5, 7, -5, -5, 7, -9, -5, -5, 7, 7, -9, 5,
          5, -7, -3, 5, -5, 7, -5, 7, -9, 5, -7, 5, -3, 5]
        upper:
          bounds: {local: '18'}
          keywords: [facet-local]
        keywords: [canonical, minimal, not-composite, not-io-lifted]
```

## RepresentationTransform

A `RepresentationTransform` changes the representation of a Bell expression. A bijective change between compatible representations is always allowed (see the table in [Representations](http://docs.faacets.com/latest/concepts/representation.html)).

Bell expressions in a representation of the no-signaling subspace can always be transformed in the full signaling space; the reverse can be done only if the expression does not contains signaling terms. Signaling terms should be removed using a `RedundantTransform`.

A `RepresentationTransform` has an `of` key describing the expression to transform, and a `representation` key with a string value corresponding to a valid representation ([Representations](http://docs.faacets.com/latest/concepts/representation.html)).

Example: the original I3322 inequality is written in the Collins-Gisin notation

``` yaml
type: RepresentationTransform
representation: Non-signaling Collins-Gisin
of:
    type: PermutationTransform
    permutation: A1(1,2) A2(1,2) A3(1,2)
    of:
        type: AffineTransform
        affine: 1/12 * x - 1
        of: {type: CanonicalExpression, index: 4}
```

## RepresentativeTransform

As an alternative way to specify a particular relabeling of a Bell expression, the `RepresentativeTransform` describe the operation of taking the lexicographic representative of rank $$k$$.

It contains an of key describing the expression to transform, and a `representative` key with the corresponding index. Representative indices are 0-based.

## Todo

Should it be 0 or 1-based ?
