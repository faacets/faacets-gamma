---
layout: docs
title:  "Example of Scala console usage"
section: "console"
---

# Example of Scala console usage

Here are some examples of using the Faacets software to perform computations through the scala console. To install this console, please see the instructions [here](../gettingStarted.html).

To start a Faacets session, always start by loading the faacets objects:
```tut
import com.faacets.core._
import com.faacets.defaults._
```

Let us look at the CHSH Bell expression in the correlators form. It can be represented by the following [table](../overview/representations.html):

$$
\begin{array}{c|cc}
0 & 0 & 0\\
\hline
0 & 1 & 1\\
0 & 1 & -1
\end{array}
$$

A vector representation of these coefficients is obtained by reading this table column after column. Thus, we can define this Bell expression in the CHSH scenario:
```tut
Expr.correlators(Scenario.CHSH, Vec[Rational](0,0,0,0,1,1,0,1,-1))
```

You can construct CHSH directly in the P(ab|xy) basis
```tut
val chsh = Expr(Scenario.CHSH, Vec[Rational](1, -1, 1, -1, -1, 1, -1, 1, 1, -1, -1, 1, -1, 1, 1, -1))
```

But you have to be in the nonsignaling subspace, otherwise there is a problem
```tut:fail
Expr(Scenario.CHSH, Vec[Rational](1, -1, 1, -1, -1, 1, -1, 1, 1, -1, -1, 1, -1, 1, 1, 0))
```

or you use the DExpr which can have signaling terms
```tut
val de = DExpr(Scenario.CHSH, Vec[Rational](1, -1, 1, -1, -1, 1, -1, 1, 1, -1, -1, 1, -1, 1, 1, 0))
```

and you can project a DExpr
```tut
de.split
```

where the first part is the nonsignaling projection
```tut
val deProj = de.split._1
```

you cannot obtain the symmetry group of an expression in the full space (incl. signaling)
```tut:fail
de.symmetryGroup
```

but you can compute it when the expression is projected
```tut
deProj.symmetryGroup
```

here, for the genuine CHSH inequality
```tut
chsh.symmetryGroup
```

when serializing as YAML, the generators are written in a "nice" form
```tut
chsh.symmetryGroup.asYaml
```
