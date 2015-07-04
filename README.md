# zipperl

An Erlang library for [zipper data structures](http://en.wikipedia.org/wiki/Zipper_%28data_structure%29).

## Modules

#### yazl 

Yet Another Zipper List (yazl), pronounced '_yazzle_'.    
A list with a current focus giving incremental bidirectional traversal and constant time read, write and insert.   

The interface is expressed symmetrically for operations in the right direction `rdir` (forwards) and left direction `ldir`  (backwards). There are markers `endl` for the left end (before first) and `endr` for the right end (after last).

For example, here is a 5-element list, with the focus at the beginning, as if created with `Yazl = from_list([1,2,3,4,5])`:

![yazl endl position 1](http://rawgit.com/mike-french/zipperl/master/doc/yazl-pos1.png)

Here is the same structure after moving the focus one step to the right, as if with a call to `move( rdir, Yazl )`:

![yazl endl position 2](http://rawgit.com/mike-french/zipperl/master/doc/yazl-pos2.png)

And another move to the right:

![yazl endl position 3](http://rawgit.com/mike-french/zipperl/master/doc/yazl-pos3.png)

Here is the empty yazl, as if created with `Yazl = new()` or `Yazl = from_list([])`:

![empty yazl](http://rawgit.com/mike-french/zipperl/master/doc/yazl-empty.png)

## Build

Build using rebar3 \[[home](http://www.rebar3.org/)\]\[[github](http://github.com/rebar/rebar3)\]

&nbsp; &nbsp; `rebar3 do deps, compile, dialyzer`

## Test

Tests are written using PropEr \[[home](http://proper.softlab.ntua.gr/)\] property-based testing, but invoked through EUnit \[[home](http://www.erlang.org/doc/apps/eunit/chapter.html)\] wrappers, so that the rebar3 `eunit` task can be used:

&nbsp; &nbsp; `rebar3 eunit`    

or invoke tests explicitly from the shell:

&nbsp; &nbsp; `erl -pz _build/test/lib/zipperl/ebin _build/test/lib/proper/ebin`    
&nbsp; &nbsp; `> proper:check_specs( yazl ).`     
&nbsp; &nbsp; `> proper:module( yazl_tests ).`    
&nbsp; &nbsp; `> yazl_tests:performance_all().`

## Documentation

There is a set of edoc already checked-in:

&nbsp; &nbsp; yazl \[[edoc](http://rawgit.com/mike-french/zipperl/master/doc/yazl.html)\]

but to regenerate use:

&nbsp; &nbsp; `rebar3 edoc`

## License

The software is Copyright © 2015 Mike French, released under the permissive MIT open source license. See [LICENSE.txt](http://rawgit.com/mike-french/zipperl/master/LICENSE.txt) for more details.

## References

\[1\] _Functional Pearl: The Zipper_, Gérard Huet, 1997 \[[pdf](http://yquem.inria.fr/~huet/PUBLIC/zip.pdf)\]    
\[2\] _The Derivative of a Regular Type is its Type of One-Hole Contexts_, Conor McBride \[[pdf](http://strictlypositive.org/diff.pdf)\]    
\[3\] _Zippers_, Pavel Pancheka \[[html](https://pavpanchekha.com/blog/zippers/huet.html)\]    
\[4\] _Yet Another Article on Zippers, in Erlang_, Fred Hébert \[[html](http://ferd.ca/yet-another-article-on-zippers.html)\]    
\[5\] _Zippers_, Fred Hébert \[[github](https://github.com/ferd/zippers)\]



