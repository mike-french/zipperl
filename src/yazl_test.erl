
-module(yazl_test).

-compile( {no_auto_import,[size/1]} ).

-import( yazl,
         [delete/2,
          ending/1,
          find/3,
          finds/3,
          foldl/4,
          foldr/4,
          from_list/1, from_list/2,
          get/2,
          insert/3, 
          inserts/3,
          is_yazl/1,
          is_empty/1,
          map/3,
          move/2, 
          moves/3,
          moveto/2,
          moveuntil/3,
          movewhile/3,
          new/0,
          opposite/1,
          position/2,
          reverse/1,
          set/3,
          size/1,
          to_list/1,
          truncate/2
         ] ).
         
-include_lib("proper/include/proper.hrl").

% ---------------------------------------------------
% Proper utility types and generators

% -type yazls :: yasl:yazl( integer() ).
% -type yazls :: { list(integer()), list(integer()) }.

yazls() -> { list(integer()), list(integer()) }.

nonempty_yazls() -> { non_empty(list(integer())), 
                      non_empty(list(integer())) }.

% ---------------------------------------------------
% Local utility functions

reflect( r,  _, endr ) -> 1;
reflect( l,  N, endl ) -> N;
reflect( r,  _, 1    ) -> endr;
reflect( l,  N, N    ) -> endl;
reflect( r,  N, IR   ) -> N - IR + 2;
reflect( l,  N, IL   ) -> N - IL.

% ---------------------------------------------------
% reverse( yazl(A) ) -> yazl(A).

prop_reverse_empty() ->
  io:fwrite( "Reversing empty yazl leaves yazl unchanged~n" ),
  new() =:= reverse( new() ).

prop_reverse_single() ->
  io:fwrite( "Reversing a singleton leaves list unchanged~n" ),
  L = [a],
  L =:= to_list( reverse( from_list(L) ) ).

prop_reverse_list() ->
  io:fwrite( "Reversing yazl reverses the underlying list~n" ),     
  ?FORALL( Z, yazls(),
     lists:reverse( to_list(Z) ) =:= to_list( reverse(Z) )
  ).
  
prop_reverse_twice_identity() ->
  io:fwrite( "Reversing twice restores the original~n" ),     
  ?FORALL( Z, yazls(),
     Z =:= reverse( reverse(Z) )
  ).
  
prop_reverse_switches_ends() ->
  io:fwrite( "Reversing an end position switches the ending~n" ),
  ?FORALL( L, list(),
     (endr =:= position( r, reverse( from_list(endl,L) ) )) and
     (endl =:= position( l, reverse( from_list(endr,L) ) ))
  ).
  
prop_reverse_switches_current_values() ->
  io:fwrite( "Reversing switches local values~n" ),
  ?FORALL( Z, nonempty_yazls(),
     begin
       LVal = get( l, Z ),
       RVal = get( r, Z ),
       RevZ = reverse( Z ),
       (LVal =:= get( r, RevZ )) and 
       (RVal =:= get( l, RevZ ))
     end
  ).
  
prop_reverse_reflects_index() ->
  io:fwrite( "Reversing reflects the index position~n" ),     
  ?FORALL( Z, nonempty_yazls(),
     begin
       N = size( Z ),
       PosR = position( r, Z ),
       PosL = position( l, Z ),
       RevZ = reverse( Z ),
       PosRevR = position( r, RevZ ),
       PosRevL = position( l, RevZ ),
       (PosRevR =:= reflect( r, N, PosR )) and
       (PosRevL =:= reflect( l, N, PosL ))
     end
  ).
  
% ---------------------------------------------------
% new() -> empty_yazl().
% is_yazl( term() ) -> boolean().

prop_new_value() ->
  io:fwrite( "An empty yazl is a tuple of 2 empty lists~n" ),
  Z = new(),
  is_yazl(Z) and (Z =:= {[],[]}).
  
prop_to_list_empty() ->
  io:fwrite( "An empty yazl gives the empty list~n" ),
  Z = new(),
  is_yazl(Z) and ([] =:= to_list(Z)). 

% ---------------------------------------------------
% from_list( [A] ) -> yazl(A).
% to_list( yazl(A) ) -> [A].

prop_from_list_to_list() ->
  io:fwrite( "Roundtrip from list gives original list~n" ),
  ?FORALL( L, list(),
    begin
      Z = from_list( L ),
      is_yazl(Z) and (L =:= to_list(Z))
    end
  ).
  
% ---------------------------------------------------
% from_list( position(), [A] ) -> yazl(A).

prop_from_list_i_to_list() ->
  io:fwrite( "Roundtrip from list with position gives original list~n" ),
  ?FORALL( L, non_empty(list()),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,L),
            is_yazl(Z) and (L =:= to_list(Z))
          end
       )
     end
  ).
  
% ---------------------------------------------------
% is_empty( yazl(_) ) -> boolean().

prop_is_empty_new() ->
  io:fwrite( "An empty yazl is empty~n" ),
  is_empty( new() ).
  
prop_is_empty_not() ->
  io:fwrite( "Non-empty lists are non-empty~n" ),
  ?FORALL( L, non_empty(list()), 
     not is_empty( from_list(L) )
  ).
  
% ---------------------------------------------------
% size( yazl(_) ) -> non_neg_integer().

prop_size_list() ->
  io:fwrite( "Size is same as length of the list~n" ),
  ?FORALL( L, list(), 
     length(L) =:= size( from_list(L) ) 
  ).

% ---------------------------------------------------
% position( direction(), yazl(_) ) -> position().

prop_position_from_endl() ->
  io:fwrite( "Import list at endl makes positions l->endl and r->1|endr~n" ),
  ?FORALL( L, list(),
     begin
       Z = from_list( endl, L ),
       RPos = case size(Z) of 0 -> endr; _N -> 1 end, 
       (endl =:= position( l, Z )) and
       (RPos =:= position( r, Z ))
     end
  ).

prop_position_from_endr() ->
  io:fwrite( "Import list at endr makes positions r->endr and l->1|endl~n" ),
  ?FORALL( L, list(),
     begin
       Z = from_list( endr, L ),
       LPos = case size(Z) of 0 -> endl; N -> N end,
       (endr =:= position( r, Z )) and
       (LPos =:= position( l, Z ))
     end
  ).
  
prop_position_from_i() ->
  io:fwrite( "Import list at i has position r->i~n" ),
  ?FORALL( L, non_empty(list()),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          I =:= position( r, from_list(I,L) )
       )
     end
  ).

% ---------------------------------------------------
% get( direction(), yazl(A) ) -> maybe(A).

% ---------------------------------------------------
% move( direction(), yazl(A) ) -> maybe(yazl(A)).
-ifdef( ZZZ ).
prop_position_from_endl() ->
  io:fwrite( "Import list at endl makes positions l->endl and r->1|endr~n" ),
  ?FORALL( L, list(),
     begin
       Z = from_list( endl, L ),
       RPos = case size(Z) of 0 -> endr; _N -> 1 end, 
       (endl =:= position( l, Z )) and
       (RPos =:= position( r, Z ))
     end
  ).

prop_position_from_endr() ->
  io:fwrite( "Import list at endr makes positions r->endr and l->1|endl~n" ),
  ?FORALL( L, list(),
     begin
       Z = from_list( endr, L ),
       LPos = case size(Z) of 0 -> endl; N -> N end,
       (endr =:= position( r, Z )) and
       (LPos =:= position( l, Z ))
     end
  ).
  
prop_position_from_i() ->
  io:fwrite( "Import list at i has position r->i~n" ),
  ?FORALL( L, non_empty(list()),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          I =:= position( r, from_list(I,L) )
       )
     end
  ).
-endif.
% ---------------------------------------------------
% moves( direction(), integer(), yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% moveto( position(), yazl(A) ) -> maybe(yazl(A)).
  
% ---------------------------------------------------
% find( direction(), A, yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% finds( direction(), [A], yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% moveuntil( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% movewhile( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% set( direction(), A, yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% insert( direction() | ending(), A, yazl(A) ) -> yazl(A).

% ---------------------------------------------------
% inserts( direction(), [A], yazl(A) ) -> yazl(A).

% ---------------------------------------------------
% delete( direction(), yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% truncate( direction(), yazl(A) ) -> yazl(A).



% ---------------------------------------------------
% map( direction(), fun((A)->B), yazl(A) ) -> [B].

% ---------------------------------------------------
% foldl( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

% ---------------------------------------------------
% foldr( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

%====================================================================

