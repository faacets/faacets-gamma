---
layout: docs
title:  "Representations"
section: "overview"
---

Representations
===============

A Bell expression is defined as a direction in the space of conditional probabilities $$P(a,b,\ldots\vert x,y,\ldots)$$, where $$a,b,\ldots$$ are the parties' individual outcomes and $$x,y,\ldots$$ their measurement settings (see [Bell scenario](scenario.html)). The description of a Bell inequality, i.e. of a Bell expression with given direction and local bound, thus relies on a choice of parametrization for the space of conditional probability distributions. A natural parametrization of this space is given by the list of all probabilities $$P(a,b,\ldots\vert x,y,\ldots)\ \forall a,b,x,y,\ldots$$ themselves. A vectorization of these parameters then gives rise to a vector-representation of a Bell expression.

However, due to the normalization of probabilities (i.e. the fact that $$\sum_{a,b}P(a,b\vert x,y)=1\ \forall x,y$$ ), as well as the no-signalling condition, this straightforward parametrization of the probability space is overcomplete. This results in a degeneracy of the Bell expression coefficients (See [FaacetsPaper](https://arxiv.org/abs/1404.1306) for more details). In order to guarantee a unique description of every Bell inequality, we rely on parametrizations of the normalized nosignaling suspace.


Non-Signaling probabilities
---------------------------
This parametrization uses the whole set of conditional probabilities $$P(a,b,\ldots\vert x,y,\ldots)$$, but removes the degeneracy in the Bell coefficients by requiring them to satisfy a set of constraints. These constraints are uniquely defined and preserve the symmetry under [relabelings](relabelings.html) of the inequality (see [here](https://arxiv.org/abs/1610.01833) for more details on this projection). A Bell expression is then described in terms of these coefficients.

Concretely, the Bell expression
<p align="center">
$$B=\sum_{a,b,x,y,\ldots} \alpha_{a,b,\ldots\vert x,y,\ldots}P(a,b,\ldots\vert x,y,\ldots)$$
</p>
is expressed in vector form by enumerating the coefficients $$\alpha_{a,b,\ldots\vert x,y,\ldots}$$. The enumeration of the coefficients increment first Alice's outcome $$a$$, then increment Alice's setting $$x$$, then increment Bob's outcome $$b$$, then increment Bob's setting $$y$$, and so on.

In the CHSH scenario with two parties and binary measurement settings/outcomes, for instance, the order of terms is: $$\alpha_{0,0\vert 0,0}$$ , $$\alpha_{1,0\vert 0,0}$$ , $$\alpha_{0,0\vert 1,0}$$ , $$\alpha_{1,0\vert 1,0}$$ , $$\alpha_{0,1\vert 0,0}$$ , $$\alpha_{1,1\vert 0,0}$$ , $$\alpha_{0,1\vert 1,0}$$ , $$\alpha_{1,1\vert 1,0}$$ , $$\alpha_{0,0\vert 0,1}$$ , $$\alpha_{1,0\vert 0,1}$$ , $$\alpha_{0,0\vert 1,1}$$ , $$\alpha_{1,0\vert 1,1}$$ , $$\alpha_{0,1\vert 0,1}$$ , $$\alpha_{1,1\vert 0,1}$$ , $$\alpha_{0,1\vert 1,1}$$ , $$\alpha_{1,1\vert 1,1}$$ .

This ordering is obtained equivalently by listing all the columns of the following table:

$$
\begin{array}{cc|cc}
\alpha_{0,0|0,0} & \alpha_{0,1|0,0} & \alpha_{0,0|0,1} & \alpha_{0,1|0,1}\\
\alpha_{1,0|0,0} & \alpha_{1,1|0,0} & \alpha_{1,0|0,1} & \alpha_{1,1|0,1}\\
\hline
\alpha_{0,0|1,0} & \alpha_{0,1|1,0} & \alpha_{0,0|1,1} & \alpha_{0,1|1,1}\\
\alpha_{1,0|1,0} & \alpha_{1,1|1,0} & \alpha_{1,0|1,1} & \alpha_{1,1|1,1}\\
\end{array}
$$

Any bipartite inequality can be represented in a similar table form. Each table dimension can be identified with a given party, Alice or Bob, and to each choice of settings $$(x,y)$$ corresponds a 2x2 block of coefficients.


Collins-Gisin representation
----------------------------
This parametrization removes the degeneracy of the Bell coefficients by relying on a non-redundant basis of the normalized no-signaling space. This basis is defined by considering all probability terms (including marginals) which do not involve the last possible outcome of any party (see [CollinsGisin](http://dx.doi.org/10.1088/0305-4470/37/5/021)).

In the CHSH scenario, for instance, any Bell expression written in this representation takes the form

<p align="center">
$$B=\gamma + \sum_{x=0}^1 \alpha^B_{1\vert x} P_A(1\vert x) + \sum_{y=0}^1 \alpha^B_{1\vert x} P_A(1\vert x) + \sum_{x,y=0}^1 \alpha^{AB}_{1,1\vert x,y} P_{AB}(1,1\vert x,y).$$
</p>

Here $$P^{AB}(a,b\vert x,y)$$ stands for the joint probability distribution (denoted $$P(a,b\vert x,y)$$ above), and $$P^A(a\vert x)=\sum_b P^{AB}(a,b\vert x,y)$$ is Alice's marginal distribution.

A table-like description of such a bipartite Bell expression is then obtained in a similar fashion:

$$
\begin{array}{c|cc}
\gamma & \alpha^B_{0|0} & \alpha^B_{0|1} \\
\hline
\alpha^A_{0|0} & \alpha^{AB}_{0,0|0,0} & \alpha^{AB}_{0,0|0,1} \\
\alpha^A_{0|1} & \alpha^{AB}_{0,0|1,0} & \alpha^{AB}_{0,0|1,1}
\end{array}
$$

Listing each column gives rise to the vector form of the Collins-Gisin representation, taking successive elements: $$\gamma$$ , $$\alpha^A_{0\vert 0}$$ , $$\alpha^A_{0\vert 1}$$ , $$\alpha^B_{0\vert 0}$$ , $$\alpha^{AB}_{0,0\vert 0,0}$$ , $$\alpha^{AB}_{0,0\vert 1,0}$$ , $$\alpha^B_{0\vert 1}$$ , $$\alpha^{AB}_{0,0\vert 0,1}$$ , $$\alpha^{AB}_{0,0\vert 1,1}$$ .


