# fred-haskell

Implementation of FRED (Flexible REpresentation of Data) for haskell

# What is FRED ?

FRED (Flexible REpresentation of Data) is a data-interchange format.
It was created with the goal to be easy for humans to read and write
but also easy to create parsers. 

It has more data types than JSON
and some features like support for metadata and tags.              

# FRED Specification and Grammar

The FRED Spec and Grammar is being developed here
[FRED Grammar and Specification](https://github.com/fred-format/grammar)

# How was implemented?

fred-haskell utilizes [Parsec](http://hackage.haskell.org/package/parsec) as the parser library and follows the spec.

# Documentation and Haddock

