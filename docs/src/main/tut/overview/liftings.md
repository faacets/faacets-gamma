---
layout: docs
title:  "Liftings"
section: "overview"
---

# Liftings

Any Bell inequality is attached to a [Bell scenario](scenario.html): different Bell scenarios admit different Bell inequalities. However, some inequalities belonging to different scenarios are strongly related to each other. The operation consisting in _lifting a Bell inequality_ to a more complete scenario gives rise to such strongly related Bell inequalities.

## Outcomes Lifting

This case concerns the situation in which an new scenario is created by the adjunction of possible outcomes to some of the settings, as in `[(2 2) (2 3)]` compared to `[(2 2) (2 2)]`.

Here, any Bell inequality defined in the simpler scenario remains valid in the larger scenario (i.e. with zero coefficients assigned to probabilities involving the new outcomes), but stronger inequalities can also be obtained. Namely, it was shown in [this paper](https://arxiv.org/abs/quant-ph/0503179) that assigning to the new outcome the same role as one of the existing outcome lead to an inequality which preserved the facet property of the original Bell inequality. The inequality obtained in this way is referred to as an _outcome lifted_ Bell inequality.

In the case where a third possible outcome is added to a two-outcome scenario, this last outcome can thus be attributed the role of any of the first two already-existing outcomes. We denote this choice by a partition `{0 1 0}` or `{0 1 1}` (the third outcome plays the same role as the first, respectively second, outcome). If this lifting only affects the second input of Bob, the global scenario reads either `[(2 2) (2 {0 1 0})]` or `[(2 2) (2 {0 1 1})]`.

By extension, it is also possible to write the full scenario in partition notation as in `[({0 1} {0 1}) ({0 1} {0 1 1})]`.

In the case of the CHSH inequality, the two liftings above respectively read (in full probability )

$$
L_{\{0,1,0\}}=\begin{array}{cc|ccc}
1 & -1 & 1 & -1 & 1\\
-1 & 1 & -1 & 1 & -1\\
\hline
1 & -1 & 1 & -1 & 1\\
-1 & 1 & -1 & 1 & -1\\
\end{array}\leq 2
$$

$$
L_{\{0,1,1\}}=\begin{array}{cc|ccc}
1 & -1 & 1 & -1 & -1\\
-1 & 1 & -1 & 1 & 1\\
\hline
1 & -1 & 1 & -1 & -1\\
-1 & 1 & -1 & 1 & 1\\
\end{array}\leq 2
$$

## Input Lifting

Another way to obtain a more complex scenario is by the adjunction of measurement settings. In this case, attributing a zero coefficient to preserves the facet property of a Bell inequality.

This operation can thus be described as the adjunction of indistinguishable outcomes: `[(2 2) (2 2 {0 0})]` when adding a third setting to Bob from the CHSH sceanario.

Here is the full probability table corresponding to lifting CHSH in this way:

$$
L_{\{0,0\}}=\begin{array}{cc|cc|cc}
1 & -1 & 1 & -1 & 0 & 0\\
-1 & 1 & -1 & 1 & 0 & 0\\
\hline
1 & -1 & 1 & -1 & 0 & 0\\
-1 & 1 & -1 & 1 & 0 & 0\\
\end{array}\leq 2
$$

## Parties Lifting

Finally, it is possible to define the Lifting of an inequality under addition of a party. This is a particular case of the [composition](products.html) between two inequalities.

