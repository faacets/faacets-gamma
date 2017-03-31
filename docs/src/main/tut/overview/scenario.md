---
layout: docs
title:  "Bell scenario"
section: "overview"
---

# Bell scenarios

In a Bell experiment, two or more _parties_ perform _measurements_ on individual systems and observe the produced _outcomes_. These parties can be numbered from $$1$$ to $$n$$ and are often named following the letters of the alphabet, as in Alice, Bob, Charlie and so on. The measurement settings and outcomes are also often referred to as _inputs_ and _outputs_.

[Bell experiment: the measurement settings and outcomes can be seen as the _inputs_ and _outputs_ of $$n$$ boxes]

Since each measurement performed by a party has a number of possible outcomes $$k\geq1$$, all the measurements performed by one party can be described by a vector containing the maximum number of outcomes of each of his measurement. Similarly, a tuple of these vectors characterize the _Bell scenario_:

``` scala
Party := (k1 k2 ...)
Scenario := [Party1 Party2 ...]
```

(notice the different brackets that distinguish a party from the whole scenario)

In general, the following notation characterizes an arbitrary Bell scenario: $$[ (k_{1,1} k_{12} \ldots k_{1, m_1})~(k_{2,1} k_{2,2} \ldots k_{2, m_2}) \ldots (k_{n, 1} k_{n, 2} \ldots k_{n, m_n})]$$ where $$m_{i\ge 1}$$ is the number of measurement settings for the $$i^\text{th}$$ party and $$k_{i,j}\ge 1$$ is the number of measurement outcomes for the $$j^\text{th}$$ setting of the $$i^\text{th}$$ party.

#### Examples:

- `[(2 2) (2 2)]` is the CHSH scenario
- `[(2 2) (2 2) (2 2)]` is the tripartite Sliwa scenario
- `[(2 2 2) (2 2 2)]` is the I3322 scenario with two parties and three measurements settings per party
- `[(3 3) (3 3)]` is the I2233 scenario with two ternary measurement per party
- `[(2 3 4) (5 6)]` is a Bell scenario with two parties in which each party can choose between a different number of measurements and all measurements can produce a different number of outcomes.
- `[(2,2), (2,2)]` leads to an error because separators are not single spaces.


## Canonical form of a Bell scenario
Notice that parties `(3 2)` and `(2 3)` have essentially the same measurement structure: the only difference lies in a reordering of the measurement settings. We thus define a party description as _canonical_ when its successive numbers of outcomes are non-increasing. The canonical form of `(2 3)` is then `(3 2)`.

Similarly, notice that scenarios `[(2 2) (3 3)]` and `[(3 3) (2 2)]` are identical up to a reordering of the parties. The canonical form of a scenario is then defined as the one in which all its parties' structures are themselves canonical and ordered lexicographically. Hence, in a canonical scenario, we have either $$m_{i}<j$$ or $$k_{i,j} \geq k_{i',j}$$ for all parties $$i,i'$$ with $$i'>i$$ and any measurement setting $$j$$. The canonical form of `[(2 3 4) (5 6)]` is thus `[(6 5) (4 3 2)]`.
