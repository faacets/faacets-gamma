function res = sumI(vec)
i = com.faacets.gluon.Interface.compile({
    'import com.faacets.gluon._'
    'import com.faacets.defaults._'
    'Interface[Seq[SafeLong], SafeLong]("sumI", "seq") {'
    'seq => seq.fold(SafeLong.one)(_+_)'
    '}'
                   });
res = i.call(vec);
