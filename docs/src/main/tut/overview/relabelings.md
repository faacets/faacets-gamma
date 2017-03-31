---
layout: docs
title:  "Relabelings"
section: "overview"
---

# Relabelings

In a Bell experiment, no preferential importance is attached to any particular party, measurement setting or outcome. Therefore, any two inequalities which can be related by a relabelling of the parties, inputs or outputs that is compatible with the Bell scenario are viewed as equivalent.

Any relabeling is a permutation of parties, inputs and outputs. As such, it can be specified by describing the permutation acting at each level.

A _permutation of parties_ is written in the [cycle notation](https://en.wikipedia.org/wiki/Permutation#Cycle_notation), using letters. For example, the permutation of Alice and Bob (the first two parties) is written `(A, B)`.

A _relabeling of measurement settings_ is specified by writing first the party letter and then permutation of settings in the cycle notation. The cyclic permutation of the three measurement settings of Alice is written `A(1,2,3)`.

A _relabeling of measurement outcomes_ is specified by writing the party and measurement setting affected first, using a letter and a number respectively, and then the permutation of outcomes itself using the cycle notation. For example, the permutation of outcomes 1 and 2 for the third measurement setting of Alice is written `A3(1,2)`.

A _relabelling_ is then expressed as a string describing successively the permutation of measurement outcomes, measurement settings and party, in that order. Each permutation being separated by a single space.

Permutation involving several cycles can be specified without repeating the permutation header, as in `A1(1,2)(3,4)`. In this case, no space is used between the parentheses.

As an example, the generators of the symmetry group of the original CHSH inequality can be written as:

- `A1(1,2) A2(1,2) B1(1,2) B2(1,2)` (permuting outcomes for all settings)
- `A2(1,2) B(1,2)`, `B1(1,2) A(1,2)` (permuting outcomes and settings at the same time)
- `A(1,2) B(1,2) (A,B)` (permuting parties and settings).

Note that permutations act as a right action on Bell expressions (see the Appendix B of [FaacetsPaper](https://arxiv.org/abs/1404.1306)). In other words, the action of a permutation $$\pi$$ on the components of a Bell expression described by the vector $$v(i)$$ with indices $$i$$ is defined as $$v^\pi(i^\pi) = v(i)$$.
