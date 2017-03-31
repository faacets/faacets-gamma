---
layout: docs
title:  "Decomposition and remarkable forms"
section: "fileFormat"
---

# Decomposition and remarkable forms

The operations that relate a Bell expression to its canonical form are stored in a YAML file to avoid expensive recomputations. The motivation for these operations is described in [FaacetsPaper](http://www.arxiv.org/).

Moreover, when storing canonical forms of Bell expressions, one wants to keep track of possible remarkable forms of an expression, for example a form symmetric under permutation of parties.

A Bell expression which is not in the canonical form can have a `decomposition` key that provide its decomposition into canonical elements. The possible elements in the `decomposition` key are described in the next section [Transforms](transform.md). When the Bell expression is included in a compendium, the canonical elements can be referenced using the `CanonicalExpression` placeholder described below.

A Bell expression, canonical or not, can have a list of remarkable forms under the key `remarkableForms`, containing a mapping of form names to decompositions. These decompositions should include as final elements the `OriginalExpression` placeholder.

Reserved keys for remarkable forms include:

- maximal

    Describes the maximal lexicographic of the Bell expression. Has to be present for canonical expressions, because it is used to compute quickly the minimal lexicographic representative of the opposite of a canonical expression.

- fullySymmetric

    Describes the representative of a Bell expression under relabelings fully symmetric under the permutation of parties. If several of such representatives exists, the form should reference the one minimal under lexicographic ordering.


## Bell Expressions placeholders

Two special Bell Expression placeholders can be used in decompositions or remarkable forms:

- CanonicalExpression

    This placeholder has a single key `index` which references a canonical Bell expression in the canonical folder of a compendium.

- OriginalExpression

    Used as a placeholder in the remarkable forms section, to reference the main Bell expression of the file.

