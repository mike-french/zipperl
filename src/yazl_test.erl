
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
         
-export( [test/0] ).

-include_lib("proper/include/proper.hrl").

% ---------------------------------------------------
% Proper utility types and generators

% -type yazls :: yasl:yazl( integer() ).
% -type yazls :: { list(integer()), list(integer()) }.

yazls() -> { list(integer()), list(integer()) }.

nonempty_yazls() -> { non_empty(list(integer())), 
                      non_empty(list(integer())) }.
                      
list1() -> ?SUCHTHAT( L, list(), length(L) > 0).
list2() -> ?SUCHTHAT( L, list(), length(L) > 1).
list3() -> ?SUCHTHAT( L, list(), length(L) > 2).

% ---------------------------------------------------
% Main test program

-spec test() -> list().

test() ->
  proper:check_specs( yazl ) ++
  proper:module( yazl_test ).

% ---------------------------------------------------
% Local utility functions

% reflect a position as if during a reverse

reflect( r,  _, endr ) -> 1;
reflect( l,  N, endl ) -> N;
reflect( r,  _, 1    ) -> endr;
reflect( l,  N, N    ) -> endl;
reflect( r,  N, IR   ) -> N - IR + 2;
reflect( l,  N, IL   ) -> N - IL.
  
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

prop_move_l_from_endl() ->
  io:fwrite( "Cannot move left from left end~n" ),
  ?FORALL( L, list(),
     endl =:= move( l, from_list( endl, L ) )
  ).
  
prop_move_r_from_endr() ->
  io:fwrite( "Cannot move right from right end~n" ),
  ?FORALL( L, list(),
     endr =:= move( r, from_list( endr, L ) )
  ).
  
prop_move_r_from_n() ->
  io:fwrite( "Move right from last element to right end~n" ),
  ?FORALL( L, list1(),
     endr =:= position( r, move( r, from_list( length(L), L ) ) )
  ).
  
prop_move_l_from_1() ->
  io:fwrite( "Move left from 2nd element to left end~n" ),
  ?FORALL( L, list2(),
     endl =:= position( l, move( r, from_list( 2, L ) ) )
  ).
  
prop_move_r_increment_r() ->
  io:fwrite( "Moving right increments right position~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N-1),
          begin
            Z = from_list(I,L),
            position( r, move(r,Z) ) =:= position(r,Z) + 1 
          end
       )
     end
  ).
  
prop_move_l_decrement_l() ->
  io:fwrite( "Moving left decrements left position~n" ),
  ?FORALL( L, list3(),
     begin
       N = length(L),
       ?FORALL( I, range(3,N),
          begin
            Z = from_list(I,L),
            position( l, move(l,Z) ) =:= position(l,Z) - 1 
          end
       )
     end
  ).
  
prop_move_r_increment_l() ->
  io:fwrite( "Moving right increments left position~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list(I,L),
            position( l, move(r,Z) ) =:= position(l,Z) + 1 
          end
       )
     end
  ).
  
prop_move_l_decrement_r() ->
  io:fwrite( "Moving left decrements right position~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(2,N-1),
          begin
            Z = from_list(I,L),
            position( r, move(l,Z) ) =:= position(r,Z) - 1 
          end
       )
     end
  ).
  
% ---------------------------------------------------
% moves( direction(), integer(), yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% moveto( position(), yazl(A) ) -> maybe(yazl(A)).
  
% ---------------------------------------------------
% find( direction(), A, yazl(A) ) -> maybe(yazl(A)).

% ---------------------------------------------------
% finds( direction(), [A], yazl(A) ) -> maybe(yazl(A)).

prop_finds_empty_list() ->
  io:fwrite( "Empty list is always found as prefix of every list~n" ),
  Z = new(),
  (Z =:= finds( r, [], Z )) and
  (Z =:= finds( l, [], Z )).
  
prop_finds_nonempty_ends() ->
  io:fwrite( "Nonempty list is not found at ends~n" ),
  ?FORALL( L, list(),
    (endr =:= finds( r, [a], from_list( endr, L ) )) and
    (endl =:= finds( l, [a], from_list( endl, L ) ))
  ).
  
prop_finds_regression1() ->
  L = [a],
  from_list(endl,L) =:= finds( l, L, from_list(endr,L) ).
  
prop_finds_regression2() ->
  L = [abc],
  from_list(endl,L) =:= finds( l, L, from_list(endr,L) ).

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
% map( direction(), fun((A)->B), yazl(A) ) -> [B].

% ---------------------------------------------------
% foldl( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

% ---------------------------------------------------
% foldr( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

%====================================================================

