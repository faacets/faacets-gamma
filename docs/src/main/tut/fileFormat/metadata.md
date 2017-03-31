---
layout: docs
title:  "File Metadata"
section: "fileFormat"
---

# File Metadata

The faacet file format does not use YAML tags to denote the types of objects. Instead, the type of an object is given by a required `type` key at the base level, associated with a string value.

For now, only Bell expressions and their derivatives can be described, so the only valid `type` at the root node is `BellExpression`.

Additional metadata can be specified through the following optional keys:

- sources
    This section can list references for the information listed in the file. It contains a mapping between properties contained in the file and their source.

    Each mapping key should correspond to the sequence of keys to traverse to obtain the property in question, such as `lower.bounds.no-signaling` or `upper.facetOf.local`.

    The mapping values are sequences of identifiers who link resources such as publications or preprints. The following identifiers are supported:

       - DOI identifiers starting with `doi:` ,
       - arXiv identifiers starting with `arXiv:`,
       - URLs starting with `http:` or `https:`.

    Here is an example of valid sources section:

``` yaml
sources:
  upper.bounds.local: ['doi:10.1007/BF02903286']
  upper.bounds.quantum: ['doi:10.1103/PhysRevA.82.022116', 'arXiv:1006.3032']
```

- shortName

    A short name for the object in the current file can be described by a short string (3-10 characters). This key is only used for objects who have a clear and widely used name, such as `CHSH` or `I3322`. Most objects in the database do not have a short name.

    Example:

``` yaml
shortName: CHSH
```

- names

    One or several names used in the litterature for the object in the current file, as a YAML sequence of strings.

    Example:

``` yaml
names: [Clauser-Horne-Shimony-Holt inequality, Clauser-Horne inequality]
```

