# zipperl

An Erlang library for [zipper data structures](http://en.wikipedia.org/wiki/Zipper_%28data_structure%29).

## Modules

#### yazl 

Yet Another Zipper List (yazl), pronounced '_yazzle_'.    
A list with a cursor giving incremental bidirectional traversal and constant time read, write and insert.     
The interface is expressed symmetrically for forward (right) and backward (left) operations.     

## Build

&nbsp; &nbsp; `rebar3 do deps, compile, dialyzer`

## Test

Tests are written using PropEr property-based testing, but invoked through EUnit wrappers, so that the rebar3 `eunit` task can be used:

&nbsp; &nbsp; `rebar3 eunit`    

or invoke PropEr explicitly from the shell:

&nbsp; &nbsp; `erl -pz _build/test/lib/zipperl/ebin _build/test/lib/proper/ebin`    
&nbsp; &nbsp; `> proper:check_specs( yazl ).`     
&nbsp; &nbsp; `> proper:module( yazl_tests ).`

## Documentation

There is a set of edoc already checked-in:

&nbsp; &nbsp; yazl \[[edoc](http://rawgit.com/mike-french/zipperl/master/doc/yazl.html)\]

but to regenerate use:

&nbsp; &nbsp; `rebar3 edoc`

## References

\[1\] _Functional Pearl: The Zipper_, Gérard Huet, 1997 \[[pdf](http://yquem.inria.fr/~huet/PUBLIC/zip.pdf)\]    
\[2\] _The Derivative of a Regular Type is its Type of One-Hole Contexts_, Conor McBride \[[pdf](http://strictlypositive.org/diff.pdf)\]    
\[3\] _Zippers_, Pavel Pancheka \[[html](https://pavpanchekha.com/blog/zippers/huet.html)\]    
\[4\] _Yet Another Article on Zippers, in Erlang_, Fred Hébert \[[html](http://ferd.ca/yet-another-article-on-zippers.html)\]    
\[5\] _Zippers_, Fred Hébert \[[GitHub](https://github.com/ferd/zippers)\]



