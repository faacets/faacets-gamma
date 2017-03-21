---
layout: docs
title:  "Representations"
section: "Overview of Bell inequalities"
position: 1
---

# Representations

Two different spaces can be used to represent joint probability
distributions:

-   joint probability distributions of the form $$P(ab...|xy...)$$ are
    contained in the correlation space,
-   local decompositions of the form $$q_{\alpha \beta ...}$$ are
    contained in the space of strategy weights.

The correlation space contains signaling and non-properly normalized
joint probability distributions. As such, we defined in
[FaacetsPaper](http://www.arxiv.org) the no-signaling subspace and its
canonical projection. Because the no-signaling subspace is not fully
dimensional, several of its parametrizations are used in the literature
and have been implemented in Faacets: the `Collins-Gisin` notation (see
[CollinsGisin](http://dx.doi.org/10.1088/0305-4470/37/5/021)), and the
binary `Correlators` notation (see e.g.
[Sliwa](http://dx.doi.org/10.1016/S0375-9601(03)01115-0)). We extended
the Correlators notation in [FaacetsPaper](https://arxiv.org/abs/1404.1306) to
non-binary outputs. We also extended both the Collins-Gisin and the
Correlators notations to include signaling terms, such that there is a
bijection between signaling probabilities \\(P(ab...|xy...)\\), the
`Signaling` `Collins-Gisin` and `Correlators` notation. The
transformation between the `Non-signaling` `probabilities`,
`Collins-Gisin` and `Correlators` notations is also bijective.

As described in [FaacetsPaper](https://arxiv.org/abs/1404.1306) , Bell expressions
can be projected in the non-signaling subspace by setting the signaling
terms to 0 in the `Collins-Gisin` or `Correlators` notations.

Local decompositions can be specified using either weights corresponding
to deterministic points, or using `strategy correlators` as specified in
the [BilocalityPaper](http://dx.doi.org/10.1103/PhysRevA.85.032119).

In summary, we distinguish:

|-------------------------------|-----------|-----------|-------------------|-------------------|
|Representation name            |Shorthand  | Bijection |    To             | Used for          |
|                               |           |   with    | represent         |                   |
|===============================|===========|===========|===================|===================|
|Signaling probabilities        |SP         |SC, SG     |Probability        |Permutation        |
|                               |           |           |distributions      |group              |
|-------------------------------|-----------|-----------|-------------------|algorithms,        |
|Non-signaling probabilities    |NP         |NC, NG     |Prob. dist.        |Canonical          |
|                               |           |           |                   |expresions         |
|-------------------------------|-----------|-----------|-------------------|-------------------|
|Signaling Collins-Gisin        |SG         |SP, SC     |Prob. dist.        |                   |
|-------------------------------|-----------|-----------|-------------------|-------------------|
|Non-signaling Collins-Gisin    |NG         |NP, NC     |Prob. dist.        |                   |
|-------------------------------|-----------|-----------|-------------------|-------------------|
|Signaling Correlators          |SC         |SP, SG     |Prob. dist.        |Product            |
|-------------------------------|-----------|-----------|-------------------|decompositions     |
|Non-signaling Correlators      |NC         |NP, NG     |Prob. dist.        |and projection     |
|                               |           |           |                   |in                 |
|                               |           |           |                   |non-signaling      |
|                               |           |           |                   |subspace           |
|-------------------------------|-----------|-----------|-------------------|-------------------|
|Strategy Correlators           |T          |W          |Local              |                   |
|                               |           |           |decompositions     |                   |
|-------------------------------|-----------|-----------|-------------------|-------------------|
|Strategy Weights               |W          |T          |Local dec.         |                   |
|-------------------------------|-----------|-----------|-------------------|-------------------|


Probabilites representations
----------------------------

In this representation, we simply write enumerate the coefficients of
the joint probability distribution $$P(ab...|xy...)$$ (or the coefficients
of a Bell expression acting on such distributions) in the following
order: we increment first Alice's outcome $$a$$, then increment Alice's
setting $$x$$, then increment Bob's outcome $$b$$, then increment Bob's
setting $$y$$, and so on.

There is no difference in the order of terms between `Signaling` and
`Non-signaling Probabilities`, except that distributions or expressions
in the `Non-signaling Probabilities` representation are non-signaling or
have been projected.

For the CHSH scenario with two parties and binary measurement
settings/outcomes, the order of terms is:
`P(11|11), P(21|11), P(11|21), P(21|21), P(12|11), P(22|11), P(12|21), P(22|21), P(11|12), P(21|12), P(11|22), P(21|22), P(12|12), P(22|12), P(12|22), P(22|22)`.

Correlators representations
---------------------------

To be described.

Collins-Gisin representations
-----------------------------

To be described.

Strategy Correlators representation
-----------------------------------

To be described.

Strategy Weights representation
-------------------------------

To be described.