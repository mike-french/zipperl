%--------------------------------------------------------------------
%
% Copyright (c) 2015 Mike French
%
% This software is released under the MIT license
% http://www.opensource.org/licenses/mit-license.php
%
%--------------------------------------------------------------------

-module( yazl_tests ).

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
          map/2,
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
         
% ---------------------------------------------------
% Libraries

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

% ---------------------------------------------------
% Debug macros to tunnel through EUnit stdout capture

-define( PRINT(MSG),       io:format( user, MSG, []   ) ).
-define( FORMAT(FMT,ARGS), io:format( user, FMT, ARGS ) ).

% ---------------------------------------------------
% Performance test parameters

-define(   SIZE, 100000 ).
-define( REPEAT,     10 ).

% ---------------------------------------------------
% EUnit wrappers

proper_spec_test_() ->
  {timeout, 300, 
    ?_assertEqual(
       [ % known errors
           {yazl,from_list,2}
       ],
       proper:check_specs( yazl ) 
    )
  }.

proper_func_test_() ->
  {timeout, 300, 
    ?_assertEqual(
      [ % known errors
      ],
      proper:module( yazl_tests )
    )
  }.
  
perf_test() ->
  delim(),
  ?FORMAT( "Performance Tests:  Size ~p   Repeat ~p~n", [?SIZE,?REPEAT] ),
  delim(),
  perf_create(),
  delim(),
  perf_move(),
  delim(),
  perf_fill(),
  delim(),
  perf_insert(),
  delim(),
  perf_map(),
  delim(),
  perf_find(),
  delim().
  
% ---------------------------------------------------
% PropEr utility types and generators

yazls() -> { list(integer()), list(integer()) }.

nonempty_yazls() -> { non_empty(list(integer())), 
                      non_empty(list(integer())) }.
                      
list1() -> [1]++list(integer()).
list2() -> [2]++list1().
list3() -> [3]++list2().

% ---------------------------------------------------
% Local utility functions

% reflect a position as if during a reverse

reflect( r,  0, endr ) -> endr;
reflect( l,  0, endl ) -> endl;
reflect( r,  _, endr ) -> 1;
reflect( l,  N, endl ) -> N;
reflect( r,  _, 1    ) -> endr;
reflect( l,  N, N    ) -> endl;
reflect( r,  N, IR   ) -> N - IR + 2;
reflect( l,  N, IL   ) -> N - IL.
  
% reflect a current value as if during a reverse

flip( endr ) -> endl;
flip( endl ) -> endr;
flip( X )    -> X.

% do multiple move

multi_move( _, 0, Z ) -> Z;
multi_move( D, I, Z ) when (I > 0) -> 
  case move(D,Z) of
    endr -> endr;
    endl -> endl;
    Next -> multi_move( D, I-1, Next )
  end.

% ===================================================
% Functional Tests

% ---------------------------------------------------
% new() -> empty_yazl().
% is_yazl( term() ) -> boolean().

prop_new_value() ->
  ?PRINT( "~nAn empty yazl is a tuple of 2 empty lists~n" ),
  Z = new(),
  is_yazl(Z) and (Z =:= {[],[]}).
  
prop_to_list_empty() ->
  ?PRINT( "An empty yazl gives the empty list~n" ),
  Z = new(),
  is_yazl(Z) and ([] =:= to_list(Z)). 

% ---------------------------------------------------
% from_list( [A] ) -> yazl(A).
% to_list( yazl(A) ) -> [A].

prop_from_list_to_list() ->
  ?PRINT( "Roundtrip from list gives original list~n" ),
  ?FORALL( L, list(),
    begin
      Z = from_list( L ),
      is_yazl(Z) and (L =:= to_list(Z))
    end
  ).
  
% ---------------------------------------------------
% from_list( position(), [A] ) -> yazl(A).
  
prop_from_list_i_to_list() ->
  ?PRINT( "Roundtrip from list with position gives original list~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,L),
            is_yazl(Z) and 
            (I =:= position(r,Z)) and
            (L =:= to_list(Z))
          end
       )
     end
  ).
  
% ---------------------------------------------------
% is_empty( yazl(_) ) -> boolean().

prop_is_empty_new() ->
  ?PRINT( "An empty yazl is empty~n" ),
  is_empty( new() ).
  
prop_is_empty_not() ->
  ?PRINT( "Non-empty lists are non-empty~n" ),
  ?FORALL( L, list1(), 
     not is_empty( from_list(L) )
  ).
  
% ---------------------------------------------------
% size( yazl(_) ) -> non_neg_integer().

prop_size_list() ->
  ?PRINT( "Size is same as length of the list~n" ),
  ?FORALL( L, list(), 
     length(L) =:= size( from_list(L) ) 
  ).

% ---------------------------------------------------
% position( direction(), yazl(_) ) -> position().

prop_position_from_endl() ->
  ?PRINT( "Import list at endl makes positions l->endl and r->1|endr~n" ),
  ?FORALL( L, list(),
     begin
       Z = from_list( endl, L ),
       RPos = case size(Z) of 0 -> endr; _N -> 1 end, 
       (endl =:= position( l, Z )) and
       (RPos =:= position( r, Z ))
     end
  ).

prop_position_from_endr() ->
  ?PRINT( "Import list at endr makes positions r->endr and l->1|endl~n" ),
  ?FORALL( L, list(),
     begin
       Z = from_list( endr, L ),
       LPos = case size(Z) of 0 -> endl; N -> N end,
       (endr =:= position( r, Z )) and
       (LPos =:= position( l, Z ))
     end
  ).
  
prop_position_from_i() ->
  ?PRINT( "Import list at i has position r->i~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          I =:= position( r, from_list(I,L) )
       )
     end
  ).

% ---------------------------------------------------
% get( direction(), yazl(A) ) -> maybe(A).
% set( direction(), A, yazl(A) ) -> maybe(yazl(A)).

prop_get_ends() ->
  ?PRINT( "Accessing past the ends does not get value~n" ),
  ?FORALL( L, list(),
     (endl =:= get( l, from_list(endl,L) )) and
     (endr =:= get( r, from_list(endr,L) ))
  ).
  
prop_get_from_position() ->
  ?PRINT( "Accessor returns the value at the index~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            V = lists:nth( I, L ),
            Z = from_list( I, L ),
            (V =:= get( r, Z )) and
            (V =:= get( l, move(r,Z) ))
          end
       )
     end
  ).
  
prop_get_set_insert_r() ->
  ?PRINT( "Accessor returns new value set or inserted~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list( I, L ),
            (99 =:= get(r,    set(r,99,Z) )) and
            (99 =:= get(r, insert(r,99,Z) )) and
            (99 =:= get(l,    set(l,99,Z) )) and
            (99 =:= get(l, insert(l,99,Z) ))
          end
       )
     end
  ).

% ---------------------------------------------------
% move( direction(), yazl(A) ) -> maybe(yazl(A)).

prop_move_l_from_endl() ->
  ?PRINT( "Cannot move left from left end~n" ),
  ?FORALL( L, list(),
     endl =:= move( l, from_list( endl, L ) )
  ).
  
prop_move_r_from_endr() ->
  ?PRINT( "Cannot move right from right end~n" ),
  ?FORALL( L, list(),
     endr =:= move( r, from_list( endr, L ) )
  ).
  
prop_move_r_from_n() ->
  ?PRINT( "Move right from last element to right end~n" ),
  ?FORALL( L, list1(),
     endr =:= position( r, move( r, from_list( length(L), L ) ) )
  ).
  
prop_move_l_from_1() ->
  ?PRINT( "Move left from 2nd element to left end~n" ),
  ?FORALL( L, list2(),
     endl =:= position( l, move( l, from_list( 2, L ) ) )
  ).
 
prop_move_r_increment_r() ->
  ?PRINT( "Moving right increments right position~n" ),
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
  ?PRINT( "Moving left decrements left position~n" ),
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
  ?PRINT( "Moving right increments left position~n" ),
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
  ?PRINT( "Moving left decrements right position~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list(I,L),
            position( r, move(l,Z) ) =:= position(r,Z) - 1 
          end
       )
     end
  ).
  
prop_move_l_r_unchanged() ->
  ?PRINT( "Moving left then right is no-op~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list(I,L),
            Z =:= move(r,move(l,Z)) 
          end
       )
     end
  ).
  
prop_move_r_l_unchanged() ->
  ?PRINT( "Moving right then left is no-op~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,L),
            Z =:= move(l,move(r,Z)) 
          end
       )
     end
  ).
  
% ---------------------------------------------------
% moves( direction(), integer(), yazl(A) ) -> maybe(yazl(A)).

prop_moves_negative_opposite() ->
  ?PRINT( "Moving -ve is same as moving +ve in other direction~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,L),
            (moves(r,I,Z) =:= moves(l,-I,Z)) and 
            (moves(l,I,Z) =:= moves(r,-I,Z))
          end
       )
     end
  ).
  
prop_moves_zero_noop() ->
  ?PRINT( "Moving by zero does not change anything~n" ),     
  ?FORALL( Z, yazls(),
     (Z =:= moves(r,0,Z)) and 
     (Z =:= moves(l,0,Z))
  ).
  
prop_moves_one_move() ->
  ?PRINT( "Moving by one is same simple move~n" ),     
  ?FORALL( Z, yazls(),
     (move(r,Z) =:= moves(r,1,Z)) and 
     (move(l,Z) =:= moves(l,1,Z))
  ).
  
prop_moves_repeated_move() ->
  ?PRINT( "Moves is the same as multiple move~n" ),
  ?FORALL( L, list3(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,L),
            ?FORALL( M, range(2,N),
              (multi_move(r,M,Z) =:= moves(r,M,Z)) and
              (multi_move(l,M,Z) =:= moves(l,M,Z))
            )
          end
       )
     end
  ).

% ---------------------------------------------------
% moveto( position(), yazl(A) ) -> maybe(yazl(A)).

prop_moveto_end() ->
  ?PRINT( "Move to end puts you at end~n" ),     
  ?FORALL( Z, yazls(),
     (endl =:= position(l,moveto(endl,Z))) and
     (endr =:= position(r,moveto(endr,Z)))
  ).
  
prop_moveto_past_end() ->
  ?PRINT( "Move to beyond the end returns end flag~n" ),     
  ?FORALL( Z, yazls(),
     (endl =:= moveto( -size(Z),Z)) and
     (endr =:= moveto(1+size(Z),Z))
  ).
  
prop_moveto_index() ->
  ?PRINT( "Move to a valid index puts you at the index~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          (from_list(I,L) =:= moveto( I, from_list(L) ) ) and
          (I =:= position( r, moveto( I, from_list(L) )))
       )
     end
  ).
  
% ---------------------------------------------------
% find( direction(), A, yazl(A) ) -> maybe(yazl(A)).

prop_find_present_r() ->
  ?PRINT( "Simple find element to right~n" ),
  L = [1,2,3],
  ZL = from_list( endl, L ),
  (from_list(1,L) =:= find( r, 1, ZL )) and
  (from_list(2,L) =:= find( r, 2, ZL )) and
  (from_list(3,L) =:= find( r, 3, ZL )).
  
prop_find_present_l() ->
  ?PRINT( "Simple find element to left~n" ),
  L = [1,2,3],
  ZR = from_list( endr, L ),
  (from_list(1,L) =:= find( l, 1, ZR )) and
  (from_list(2,L) =:= find( l, 2, ZR )) and
  (from_list(3,L) =:= find( l, 3, ZR )).
  
prop_find_absent() ->
  ?PRINT( "Cannot find absent element~n" ),
  ?FORALL( L99, list(),
     begin
       L = lists:filter( fun(X) -> X =/= 99 end, L99 ),
       ZL = from_list( endl, L ),
       ZR = from_list( endr, L ),
       (endr =:= find( r, 99, ZR )) and
       (endr =:= find( r, 99, ZL )) and
       (endl =:= find( l, 99, ZR )) and
       (endl =:= find( l, 99, ZL ))
      end
  ).

prop_find_look_the_other_way() ->
  ?PRINT( "Cannot find element by looking the other way~n" ),
  L = [1,2,3],
  ZL = from_list( endl, L ),
  ZR = from_list( endr, L ),
  (endr =:= find( r, 2, ZR )) and
  (endl =:= find( l, 2, ZL )).
  
% ---------------------------------------------------
% finds( direction(), [A], yazl(A) ) -> maybe(yazl(A)).

prop_finds_empty_list() ->
  ?PRINT( "Empty list is always found as prefix of every list~n" ),
  Z = new(),
  (Z =:= finds( r, [], Z )) and
  (Z =:= finds( l, [], Z )).
  
prop_finds_nonempty_ends() ->
  ?PRINT( "Nonempty list is not found at ends~n" ),
  ?FORALL( L, list(),
    (endr =:= finds( r, [a], from_list( endr, L ) )) and
    (endl =:= finds( l, [a], from_list( endl, L ) ))
  ).
  
prop_finds_singleton() ->
  L = [a],
  from_list(endl,L) =:= finds( l, L, from_list(endr,L) ).
  
prop_finds_whole_list() ->
  L = [abc],
  ZL = from_list(endl,L),
  ZR = from_list(endr,L),
  (ZL =:= finds( r, L, ZL )) and
  (ZL =:= finds( l, L, ZR )).

% ---------------------------------------------------
% moveuntil( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

% TODO

% ---------------------------------------------------
% movewhile( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

% TODO

% ---------------------------------------------------
% insert( direction() | ending(), A, yazl(A) ) -> yazl(A).

prop_insert_ends() ->
  ?PRINT( "Insert at the ends~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list( I, L ),
            (99 =:= get(l, moveto(endr,insert(endr,99,Z)))) and
            (99 =:= get(r, moveto(endl,insert(endl,99,Z))))
          end
       )
     end
  ).

% ---------------------------------------------------
% inserts( direction(), [A], yazl(A) ) -> yazl(A).

% TODO

% ---------------------------------------------------
% delete( direction(), yazl(A) ) -> maybe(yazl(A)).

prop_delete_ends() ->
  ?PRINT( "Deleting past the ends does not succeed~n" ),
  ?FORALL( L, list(),
     (endl =:= delete( l, from_list(endl,L) )) and
     (endr =:= delete( r, from_list(endr,L) ))
  ).

prop_delete_removes_r_element() ->
  ?PRINT( "Delete removes right element~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list( I, L ),
            lists:sublist(L,I-1) ++ lists:nthtail(I,L) =:= 
              to_list( delete( r, Z ) )
          end
       )
     end
  ).
  
prop_delete_removes_l_element() ->
  ?PRINT( "Delete removes left element~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list( I, L ),
            lists:sublist(L,I-2) ++ lists:nthtail(I-1,L) =:= 
              to_list( delete( l, Z ) )
          end
       )
     end
  ).
  
prop_delete_removes_insert() ->
  ?PRINT( "Delete removes element~n" ),
  ?FORALL( L, list2(),
     begin
       N = length(L),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list( I, L ),
            (Z =:= delete( r, insert( r, 99, Z ))) and
            (Z =:= delete( l, insert( l, 99, Z )))
          end
       )
     end
  ).

% ---------------------------------------------------
% truncate( direction(), yazl(A) ) -> yazl(A).

prop_truncate_empty() ->
  ?PRINT( "Truncate empty yazl is no-op~n" ),
  Z = new(),
  (Z =:= truncate(r,Z)) and
  (Z =:= truncate(l,Z)).

prop_truncate_at_r_end() ->
  ?PRINT( "Truncate at right end is no-op~n" ),
  ?FORALL( L, list1(),
     begin
       Z = from_list( endr, L ),
       Z =:= truncate( r, Z )
     end
  ).
  
prop_truncate_at_l_end() ->
  ?PRINT( "Truncate at left end is no-op~n" ),
  ?FORALL( L, list1(),
     begin
       Z = from_list( endl, L ),
       Z =:= truncate( l, Z )
     end
  ).
  
prop_truncate_end_position() ->
  ?PRINT( "Truncate always creates an end position~n" ),
  ?FORALL( Z, nonempty_yazls(),
     (endl =:= position( l, truncate( l, Z ))) and
     (endr =:= position( r, truncate( r, Z )))
  ).

% ---------------------------------------------------
% reverse( yazl(A) ) -> yazl(A).

prop_reverse_empty() ->
  ?PRINT( "Reversing empty yazl leaves yazl unchanged~n" ),
  new() =:= reverse( new() ).

prop_reverse_single() ->
  ?PRINT( "Reversing a singleton leaves list unchanged~n" ),
  L = [a],
  L =:= to_list( reverse( from_list(L) ) ).

prop_reverse_list() ->
  ?PRINT( "Reversing yazl reverses the underlying list~n" ),     
  ?FORALL( Z, yazls(),
     lists:reverse( to_list(Z) ) =:= to_list( reverse(Z) )
  ).
  
prop_reverse_twice_identity() ->
  ?PRINT( "Reversing twice restores the original~n" ),     
  ?FORALL( Z, yazls(),
     Z =:= reverse( reverse(Z) )
  ).
  
prop_reverse_switches_ends() ->
  ?PRINT( "Reversing an end position switches the ending~n" ),
  ?FORALL( L, list(),
     (endr =:= position( r, reverse( from_list(endl,L) ) )) and
     (endl =:= position( l, reverse( from_list(endr,L) ) ))
  ).
  
prop_reverse_switches_current_values() ->
  ?PRINT( "Reversing switches local values~n" ),
  ?FORALL( Z, yazls(),
     begin
       LVal = get( l, Z ),
       RVal = get( r, Z ),
       RevZ = reverse( Z ),
       (LVal =:= flip( get( r, RevZ ))) and 
       (RVal =:= flip( get( l, RevZ )))
     end
  ).
  
prop_reverse_reflects_index() ->
  ?PRINT( "Reversing reflects the index position~n" ),     
  ?FORALL( Z, yazls(),
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
% map( fun((A)->B), yazl(A) ) -> [B].

prop_map_empty() ->
  ?PRINT( "Map empty is empty~n" ),
  F = fun(X) -> 2*X end,
  Z = new(),
  is_empty(Z) and (Z =:= yazl:map(F,Z)).
  
prop_map_as_list() ->
  ?PRINT( "Map is same as map on lists~n" ),
  ?FORALL( L, list1(),
     begin
       N = length(L),
       ?FORALL( I, range(1,N),
          begin
            F = fun(X) -> 2*X end,
            Z = from_list( I, L ),
            lists:map(F,L) =:= to_list( map(F,Z) )
          end
       )
     end
  ).

% ===================================================
% Performance Tests

perf_create() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  perf_run( "Create - list        ",  
             fun lists:seq/2,  [1, Len] ),
  perf_run( "Create - array uninit",  
             fun array:new/1,  [Len] ),
  perf_run( "Create - array   init",  
             fun init_array/1, [Len] ),
  perf_run( "Create - yazl   first",  
             fun yazl:from_list/2, [1,        List] ),
  perf_run( "Create - yazl  middle",  
             fun yazl:from_list/2, [Len div 2,List] ),
  perf_run( "Create - yazl    last",  
             fun yazl:from_list/2, [Len,      List] ),
  perf_run( "Create - yazl    endr",  
             fun yazl:from_list/2, [endr,     List] ).
  
perf_move() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  Array = init_array( Len ),
  perf_run( "Move -  list",  
             fun traverse_list/1,  [List] ),
  perf_run( "Move - array",  
             fun traverse_array/3, [Array, Len, 0] ),
  perf_run( "Move -  yazl",  
             fun yazl:moves/3,     [r, Len, from_list(List)] ).

perf_fill() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  Array = init_array( Len ),
  perf_run( "Fill - array", 
             fun fill_array/4, [Array, 99, Len, 0] ),
  perf_run( "Fill -  yazl", 
             fun fill_yazl/3,  [r, 99, from_list(List)] ).

perf_insert() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  perf_run( "Insert - yazl", fun inflate/3, 
             [r,Len-1,from_list(List)] ).

perf_map() ->
  Len     = ?SIZE,
  List    = lists:seq( 1, Len ),
  Array   = init_array( Len ),
  Double  = fun(     X ) -> 2*X end,
  IDouble = fun( _I, X ) -> 2*X end,
  perf_run( "Map - array", fun array:map/2, 
             [IDouble, Array] ),
  perf_run( "Map -  yazl", fun yazl:map/2,       
             [Double, from_list(List)] ).

perf_find() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  Array = init_array( Len ),
  perf_run( "Find - array", fun find_array/4, 
             [Array, ?SIZE, Len, 0] ),
  perf_run( "Find -  yazl", fun yazl:find/3, 
             [r, ?SIZE,                  from_list(List)] ),
  perf_run( "Find -  yazl", fun yazl:find/3, 
             [r, fun(X) -> X==?SIZE end, from_list(List)] ).
             
%--------------------------------------------------------------
% Perf test utilities

delim() -> ?PRINT( "---------------------~n" ).

init_array( Len ) ->
  array:map( fun(I,_X) -> I+1 end, array:new(Len) ).

traverse_list( []    ) -> ok;
traverse_list( [H|T] ) ->
  H,
  traverse_list( T ).

traverse_array( _Array, N, N ) -> ok;
traverse_array(  Array, N, I ) ->
  array:get( I, Array ),
  traverse_array( Array, N, I+1 ).

fill_array( _Array,_V, N, N ) -> ok;
fill_array(  Array, V, N, I ) ->
  array:set( I, V, Array ),
  fill_array( Array, V, N, I+1 ).

% faster with throw out of a fold ?
find_array( _Array,_W, N, N ) -> endr;
find_array(  Array, W, N, I ) ->
  case array:get( I, Array ) of
    W -> I;
    _ -> find_array( Array, W, N, I+1 )
  end.

fill_yazl( Dir, V, Z ) ->
  set( Dir, V, Z ),
  case move( Dir, Z ) of
    endr -> endr;
    Z1   -> fill_yazl( Dir, V, Z1 )
  end.

inflate( Dir, V, Z ) ->
  Z1 = insert( opposite(Dir), V, Z ),
  case move( Dir, Z1 ) of
    endr -> endr;
    Z2   -> inflate( Dir, V, Z2 )
  end.

%--------------------------------------------------------------
% Main performance test wrapper and timer

perf_run( Title, Fun, Args ) ->

  ?FORMAT( "Test: ~s~n", [Title] ),

  { Time, Memory } = 
    lists:foldl(
      fun( _I, { Elapsed, Memory } ) ->
        erlang:garbage_collect(),
        { memory, Mem1 } = erlang:process_info( self(), memory ),
        Now1 = erlang:now(),
        erlang:apply( Fun, Args ),
        Now2 = erlang:now(),
        { memory, Mem2 } = erlang:process_info( self(), memory ),
        { Elapsed+timer:now_diff(Now2,Now1)/1000000, Memory+Mem2-Mem1 }
      end,
      { 0.0, 0 },
      lists:seq( 1, ?REPEAT )
    ),

  ?FORMAT( "  Elapsed time: ~f~n", [Time    /  ?REPEAT] ),
  ?FORMAT( "  Memory delta: ~b~n", [Memory div ?REPEAT] ).

%--------------------------------------------------------------