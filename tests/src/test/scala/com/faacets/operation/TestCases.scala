/*
partition is defined from the subelements of the decomposition,
do not appear necessarily in the polynomial

invalid:

"4 + 2 * A + AxCB" (parties should have lexicographical order in group)


"ABxCD + ACxBD" (groups should define a partition of parties)

valid

"2 + 2*A + AxBC + BC"

"4 + 2 * A + BCxA"

"ABxDE" (without C is allowed, but C has to be defined as a subelement)

*/