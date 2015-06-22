# zipperl

An Erlang library for [zipper data structures](http://en.wikipedia.org/wiki/Zipper_%28data_structure%29).

## Modules

#### yazl 

Yet Another Zipper List (yazl), pronounced '_yazzle_'.

A list with a cursor giving incremental bidirectional traversal and constant time read, write and insert. The interface is expressed symmetrically for forward and backward operations.

## Build

`rebar3 do deps, compile, edoc, dialyzer`

## Test

`erl shell`    
or     
`erl -pz _build/default/lib/zipperl/ebin _build/default/lib/proper/ebin`   

then 

`> yazl_test:test( specs ).`    for PropEr testing of type signatures     
`> yazl_test:test( funcs ).`    for PropEr testinf of functions     
`> yazl_test:test().`           for combined testing of specs and funcs

## References

\[1\] _Functional Pearl: The Zipper_, Gérard Huet, 1997 \[[PDF](http://yquem.inria.fr/~huet/PUBLIC/zip.pdf)\]    
\[2\] _The Derivative of a Regular Type is its Type of One-Hole Contexts_, Conor McBride \[[PDF](http://strictlypositive.org/diff.pdf)\]    
\[3\] _Zippers_, Pavel Pancheka \[[html](https://pavpanchekha.com/blog/zippers/huet.html)\]    
\[4\] _Yet Another Article on Zippers, in Erlang_, Fred Hébert \[[html](http://ferd.ca/yet-another-article-on-zippers.html)\]    
\[5\] _Zippers_, Fred Hébert \[[GitHub](https://github.com/ferd/zippers)\]



