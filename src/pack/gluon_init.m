global gluon_loaded;
if (~exist('gluon_loaded')) || (~isequal(gluon_loaded, 1))
    tic;
    disp('Gluon: adding JARs to path');
    javaaddpath([pwd '/lib/scala-compiler-2.12.1.jar']);
    javaaddpath([pwd '/lib/scala-library-2.12.1.jar']);
    javaaddpath([pwd '/lib/scala-reflect-2.12.1.jar']);
    javaaddpath([pwd '/lib/scala-xml_2.12-1.0.6.jar']);
    % look for all Gluon releases in the current directory
    % and load the most recent
    gluonjarfiles = dir('lib/gluon_2.12*.jar');
    gluonjarnames = sort({gluonjarfiles.name});
    javaaddpath(['lib/' gluonjarnames{length(gluonjarnames)}]);
    disp('Gluon: initializing');
    import com.faacets.gluon.*
    i = Interface.compile(Interface.squareCode);
    disp('Gluon: checking');
    assert(i.call(2) == 4);
    gluon_loaded = 1;
    disp(sprintf('Gluon: loaded in %.2f seconds.', toc));
end
