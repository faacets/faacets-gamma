function res = lexicoMinCorr2(bipartite_correlators)
% lexicoMinCorr2 - finds the lexicographic minimal representative of an expression
%
% While the expression is given as a (bipartite) correlator table,
% the search for the lexicographic minimal representative is done
% in the P(ab|xy) space, and the resulting expression is converted
% back in correlator format
%
% Example (CHSH): lexicoMinCorr([0 0 0; 0 1 1; 0 1 -1])
%
% The number of rows is equal to the number of inputs for Alice + 1,
% the number of columns is equal to the number of inputs for Bob + 1.
% Coefficients have to be integers. Outputs are all binary, and
% only bipartite scenarios are supported.
    nA = size(bipartite_correlators, 1) - 1;
    nB = size(bipartite_correlators, 2) - 1;
    coeffs = bipartite_correlators(:);
    i = com.faacets.gluon.Interface.compile({
        'import com.faacets.gluon._'
        'import com.faacets.defaults._'
        'import com.faacets.core._'
        'import com.faacets.operation._'
        'Interface[Int, Int, Seq[SafeLong], Seq[SafeLong]]("lexicoMin", "nA", "nB", "coeffs") {'
        '(nA, nB, coeffs) =>'
        'val scenario = Scenario(Seq(Party(Seq.fill(nA)(2)), Party(Seq.fill(nB)(2))))'
        'val expr = Expr.correlators(scenario, Vec.fromSeq(coeffs.map(Rational(_))))'
        'val lexicoMin = OperationExtractor[Expr, Relabeling].forceExtract(expr).extracted'
        'lexicoMin.correlators.map(_.numerator).toIndexedSeq'
        '}'
                   });
    res = reshape(i.call3(nA, nB, coeffs), nA+1, nB+1);
