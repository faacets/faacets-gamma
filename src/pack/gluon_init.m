global gluon_loaded;
if (~exist('gluon_loaded')) || (~isequal(gluon_loaded, 1))
    tic;
    disp('Gluon: adding JARs to path');
    files = dir([pwd '/lib/*.jar']);
    if length(files) == 0
        error('Please run gluon_init when the current path has a lib/ subfolder with the required JARs');
    end
    for i = 1:length(files)
	jarname = files(i).name;
        javaaddpath([pwd '/lib/' jarname]);
    end
    disp('Gluon: initializing');
    import com.faacets.gluon.*
    i = Interface.compile(Interface.squareCode);
    disp('Gluon: checking');
    assert(i.call(2) == 4);
    gluon_loaded = 1;
    disp(sprintf('Gluon: loaded in %.2f seconds.', toc));
end
