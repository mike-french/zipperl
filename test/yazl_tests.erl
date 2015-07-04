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

-export( [performance_all/0] ).
   
-import( yazl, [
    delete/2,
    ending/1,
    find/3,
    finds/3,
    foldl/4,
    foldr/4,
    from_list/1, from_list/2,
    from_lists/2,
    get/2,
    gets/3,
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
    sets/3,
    size/1,
    to_list/1,
    to_lists/1,
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
% EUnit wrappers

proper_spec_test_() ->
  {timeout, 300, 
    ?_assertEqual(
       [ % known errors
           {yazl,gets,3},
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
  
perf_test_() ->
  {timeout, 300, fun performance_all/0 }.
  
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

reflect( rdir,  0, endr ) -> endr;
reflect( ldir,  0, endl ) -> endl;
reflect( rdir,  _, endr ) -> 1;
reflect( ldir,  N, endl ) -> N;
reflect( rdir,  _, 1    ) -> endr;
reflect( ldir,  N, N    ) -> endl;
reflect( rdir,  N, IR   ) -> N - IR + 2;
reflect( ldir,  N, IL   ) -> N - IL.
  
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
  ?FORALL( List, list(),
    begin
      Z = from_list( List ),
      is_yazl(Z) and (List =:= to_list(Z))
    end
  ).
  
% ---------------------------------------------------
% from_list( position(), [A] ) -> yazl(A).
  
prop_from_list_i_to_list() ->
  ?PRINT( "Roundtrip from list with position gives original list~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,List),
            is_yazl(Z) and 
            (I =:= position(rdir,Z)) and
            (List =:= to_list(Z))
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
  ?FORALL( List, list1(), 
     not is_empty( from_list(List) )
  ).
  
% ---------------------------------------------------
% size( yazl(_) ) -> non_neg_integer().

prop_size_list() ->
  ?PRINT( "Size is same as length of the list~n" ),
  ?FORALL( List, list(), 
     length(List) =:= size( from_list(List) ) 
  ).

% ---------------------------------------------------
% position( direction(), yazl(_) ) -> position().

prop_position_from_endl() ->
  ?PRINT( "Import list at endl makes positions l->endl and r->1|endr~n" ),
  ?FORALL( List, list(),
     begin
       Z = from_list( endl, List ),
       RPos = case size(Z) of 0 -> endr; _N -> 1 end, 
       (endl =:= position(ldir,Z)) and
       (RPos =:= position(rdir,Z))
     end
  ).

prop_position_from_endr() ->
  ?PRINT( "Import list at endr makes positions r->endr and l->1|endl~n" ),
  ?FORALL( List, list(),
     begin
       Z = from_list( endr, List ),
       LPos = case size(Z) of 0 -> endl; N -> N end,
       (endr =:= position(rdir,Z)) and
       (LPos =:= position(ldir,Z))
     end
  ).
  
prop_position_from_i() ->
  ?PRINT( "Import list at i has position r->i~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          I =:= position( rdir, from_list(I,List) )
       )
     end
  ).

% ---------------------------------------------------
% get( direction(), yazl(A) ) -> maybe(A).
% set( direction(), A, yazl(A) ) -> maybe(yazl(A)).

prop_get_ends() ->
  ?PRINT( "Accessing past the ends does not get value~n" ),
  ?FORALL( List, list(),
     (endl =:= get( ldir, from_list(endl,List) )) and
     (endr =:= get( rdir, from_list(endr,List) ))
  ).
  
prop_get_from_position() ->
  ?PRINT( "Accessor returns the value at the index~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            V = lists:nth( I, List ),
            Z = from_list( I, List ),
            (V =:= get( rdir, Z )) and
            (V =:= get( ldir, move(rdir,Z) ))
          end
       )
     end
  ).
  
prop_get_set_insert_r() ->
  ?PRINT( "Accessor returns new value set or inserted~n" ),
  ?FORALL( List, list2(),
     begin
       N = length(List),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list( I, List ),
            (99 =:= get(rdir,    set(rdir,99,Z) )) and
            (99 =:= get(rdir, insert(rdir,99,Z) )) and
            (99 =:= get(ldir,    set(ldir,99,Z) )) and
            (99 =:= get(ldir, insert(ldir,99,Z) ))
          end
       )
     end
  ).

% ---------------------------------------------------
% -spec gets( direction(), integer(), yazl(A) ) -> [A].

prop_gets_ends() ->
  ?PRINT( "Cannot gets more elements than exist~n" ),
  List = [1,2,3,4,5],
  (endr =:= gets( rdir, 1, from_list(endr,List) )) and
  (endr =:= gets( rdir, 2, from_list(5,List)    )) and
  (endr =:= gets( rdir, 3, from_list(4,List)    )) and
  (endl =:= gets( ldir, 1, from_list(endl,List) )) and
  (endl =:= gets( ldir, 2, from_list(2,List)    )) and
  (endl =:= gets( ldir, 3, from_list(3,List)    )).
  
prop_gets_r() ->
  ?PRINT( "Simple gets elements to the right~n" ),
  List = [1,2,3,4,5],
  Z = from_list( 3, List ),
  ([]      =:= gets( rdir, 0, Z )) and
  ([3]     =:= gets( rdir, 1, Z )) and
  ([3,4]   =:= gets( rdir, 2, Z )) and
  ([3,4,5] =:= gets( rdir, 3, Z )).
  
prop_gets_l() ->
  ?PRINT( "Simple gets elements to the left~n" ),
  List = [1,2,3,4,5],
  Z = from_list( 4, List ),
  ([]      =:= gets( ldir, 0, Z )) and
  ([3]     =:= gets( ldir, 1, Z )) and
  ([2,3]   =:= gets( ldir, 2, Z )) and
  ([1,2,3] =:= gets( ldir, 3, Z )).
  
% ---------------------------------------------------
% move( direction(), yazl(A) ) -> maybe(yazl(A)).

prop_move_l_from_endl() ->
  ?PRINT( "Cannot move left from left end~n" ),
  ?FORALL( List, list(),
     endl =:= move( ldir, from_list(endl,List) )
  ).
  
prop_move_r_from_endr() ->
  ?PRINT( "Cannot move right from right end~n" ),
  ?FORALL( List, list(),
     endr =:= move( rdir, from_list(endr,List) )
  ).
  
prop_move_r_from_n() ->
  ?PRINT( "Move right from last element to right end~n" ),
  ?FORALL( List, list1(),
     endr =:= position( rdir, move( rdir, from_list(length(List),List) ) )
  ).
  
prop_move_l_from_1() ->
  ?PRINT( "Move left from 2nd element to left end~n" ),
  ?FORALL( List, list2(),
     endl =:= position( ldir, move( ldir, from_list(2,List) ) )
  ).
 
prop_move_r_increment_r() ->
  ?PRINT( "Moving right increments right position~n" ),
  ?FORALL( List, list2(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N-1),
          begin
            Z = from_list(I,List),
            position( rdir, move(rdir,Z) ) =:= position(rdir,Z) + 1 
          end
       )
     end
  ).
  
prop_move_l_decrement_l() ->
  ?PRINT( "Moving left decrements left position~n" ),
  ?FORALL( List, list3(),
     begin
       N = length(List),
       ?FORALL( I, range(3,N),
          begin
            Z = from_list(I,List),
            position( ldir, move(ldir,Z) ) =:= position(ldir,Z) - 1 
          end
       )
     end
  ).
  
prop_move_r_increment_l() ->
  ?PRINT( "Moving right increments left position~n" ),
  ?FORALL( List, list2(),
     begin
       N = length(List),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list(I,List),
            position( ldir, move(rdir,Z) ) =:= position(ldir,Z) + 1 
          end
       )
     end
  ).
  
prop_move_l_decrement_r() ->
  ?PRINT( "Moving left decrements right position~n" ),
  ?FORALL( List, list2(),
     begin
       N = length(List),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list(I,List),
            position( rdir, move(ldir,Z) ) =:= position(rdir,Z) - 1 
          end
       )
     end
  ).
  
prop_move_l_r_unchanged() ->
  ?PRINT( "Moving left then right is no-op~n" ),
  ?FORALL( List, list2(),
     begin
       N = length(List),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list(I,List),
            Z =:= move( rdir,move(ldir,Z) ) 
          end
       )
     end
  ).
  
prop_move_r_l_unchanged() ->
  ?PRINT( "Moving right then left is no-op~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,List),
            Z =:= move( ldir, move(rdir,Z) ) 
          end
       )
     end
  ).
  
% ---------------------------------------------------
% moves( direction(), integer(), yazl(A) ) -> maybe(yazl(A)).

prop_moves_negative_opposite() ->
  ?PRINT( "Moving -ve is same as moving +ve in other direction~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,List),
            (moves(rdir,I,Z) =:= moves(ldir,-I,Z)) and 
            (moves(ldir,I,Z) =:= moves(rdir,-I,Z))
          end
       )
     end
  ).
  
prop_moves_zero_noop() ->
  ?PRINT( "Moving by zero does not change anything~n" ),     
  ?FORALL( Z, yazls(),
     (Z =:= moves(rdir,0,Z)) and 
     (Z =:= moves(ldir,0,Z))
  ).
  
prop_moves_one_move() ->
  ?PRINT( "Moving by one is same simple move~n" ),     
  ?FORALL( Z, yazls(),
     (move(rdir,Z) =:= moves(rdir,1,Z)) and 
     (move(ldir,Z) =:= moves(ldir,1,Z))
  ).
  
prop_moves_repeated_move() ->
  ?PRINT( "Moves is the same as multiple move~n" ),
  ?FORALL( List, list3(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list(I,List),
            ?FORALL( M, range(2,N),
              (multi_move(rdir,M,Z) =:= moves(rdir,M,Z)) and
              (multi_move(ldir,M,Z) =:= moves(ldir,M,Z))
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
     (endl =:= position( ldir, moveto(endl,Z) )) and
     (endr =:= position( rdir, moveto(endr,Z) ))
  ).
  
prop_moveto_past_end() ->
  ?PRINT( "Move to beyond the end returns end flag~n" ),     
  ?FORALL( Z, yazls(),
     (endl =:= moveto( -size(Z),Z)) and
     (endr =:= moveto(1+size(Z),Z))
  ).
  
prop_moveto_index() ->
  ?PRINT( "Move to a valid index puts you at the index~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          (from_list(I,List) =:= moveto( I, from_list(List) ) ) and
          (I =:= position( rdir, moveto( I, from_list(List) )))
       )
     end
  ).
  
% ---------------------------------------------------
% find( direction(), A, yazl(A) ) -> maybe(yazl(A)).

prop_find_present_r() ->
  ?PRINT( "Simple find element to right~n" ),
  List = [1,2,3],
  Z = from_list( endl, List ),
  (from_list(1,List) =:= find( rdir, 1, Z )) and
  (from_list(2,List) =:= find( rdir, 2, Z )) and
  (from_list(3,List) =:= find( rdir, 3, Z )).
  
prop_find_present_l() ->
  ?PRINT( "Simple find element to left~n" ),
  List = [1,2,3],
  ZR = from_list( endr, List ),
  (from_list(2,List)    =:= find( ldir, 1, ZR )) and
  (from_list(3,List)    =:= find( ldir, 2, ZR )) and
  (from_list(endr,List) =:= find( ldir, 3, ZR )).
  
prop_find_absent() ->
  ?PRINT( "Cannot find absent element~n" ),
  ?FORALL( List99, list(),
     begin
       List = lists:filter( fun(X) -> X =/= 99 end, List99 ),
       ZL = from_list( endl, List ),
       ZR = from_list( endr, List ),
       (endr =:= find( rdir, 99, ZR )) and
       (endr =:= find( rdir, 99, ZL )) and
       (endl =:= find( ldir, 99, ZR )) and
       (endl =:= find( ldir, 99, ZL ))
      end
  ).

prop_find_look_the_other_way() ->
  ?PRINT( "Cannot find element by looking the other way~n" ),
  List = [1,2,3],
  ZL = from_list( endl, List ),
  ZR = from_list( endr, List ),
  (endr =:= find( rdir, 2, ZR )) and
  (endl =:= find( ldir, 2, ZL )).
  
% ---------------------------------------------------
% finds( direction(), [A], yazl(A) ) -> maybe(yazl(A)).

prop_finds_empty_list() ->
  ?PRINT( "Empty list is always found as prefix of every list~n" ),
  Z = new(),
  (Z =:= finds( rdir, [], Z )) and
  (Z =:= finds( ldir, [], Z )).
  
prop_finds_nonempty_ends() ->
  ?PRINT( "Nonempty list is not found at ends~n" ),
  ?FORALL( List, list(),
    (endr =:= finds( rdir, [a], from_list(endr,List) )) and
    (endl =:= finds( ldir, [a], from_list(endl,List) ))
  ).
  
prop_finds_singleton() ->
  List = [a],
  ZL = from_list(endl,List),
  ZR = from_list(endr,List),
  (ZL =:= finds( rdir, List, ZL )) and
  (ZR =:= finds( ldir, List, ZR )).
  
prop_finds_whole_list() ->
  List = [a,b,c],
  ZL = from_list(endl,List),
  ZR = from_list(endr,List),
  (ZL =:= finds( rdir, List, ZL )) and
  (ZR =:= finds( ldir, List, ZR )).

% ---------------------------------------------------
% moveuntil( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

prop_moveuntil_empty() ->
  ?PRINT( "Move until on empty yazl does not succeed~n" ),
  Fun = fun(X) -> X =:= z end,
  (endr =:= moveuntil( rdir, Fun, new() )) and 
  (endl =:= moveuntil( ldir, Fun, new() )).
  
prop_moveuntil_never() ->
  ?PRINT( "Move until false predicate does not succeed~n" ),
  List = [a,b,c],
  ZL = from_list(endl,List),
  ZR = from_list(endr,List),
  Fun = fun(X) -> X =:= z end,
  (endr =:= moveuntil( rdir, Fun, ZL )) and 
  (endl =:= moveuntil( ldir, Fun, ZR )).
  
prop_moveuntil_start() ->
  ?PRINT( "Move until finding first element gives original yazl~n" ),
  List = [a,b,c],
  ZL = from_list(endl,List),
  ZR = from_list(endr,List),
  Fun = fun(X) -> X =:= a end,
  LtoR = moveuntil( rdir, Fun, ZL ),
  RtoL = moveuntil( ldir, Fun, ZR ),
  (          ZL  =:= LtoR) and Fun( get(rdir,LtoR) ) and
  (move(rdir,ZL) =:= RtoL) and Fun( get(ldir,RtoL) ).
  
prop_moveuntil_middle() ->
  ?PRINT( "Move until finding middle element gives moved yazl~n" ),
  List = [a,b,c],
  ZL = from_list(endl,List),
  ZR = from_list(endr,List),
  Zb = from_list(2,List),
  Fun = fun(X) -> X =:= b end,
  LtoR = moveuntil( rdir, Fun, ZL ),
  RtoL = moveuntil( ldir, Fun, ZR ),
  (          Zb  =:= LtoR) and Fun( get(rdir,LtoR) ) and
  (move(rdir,Zb) =:= RtoL) and Fun( get(ldir,RtoL) ).
  
% ---------------------------------------------------
% movewhile( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

% TODO

% ---------------------------------------------------
% insert( direction() | ending(), A, yazl(A) ) -> yazl(A).

prop_insert_ends() ->
  ?PRINT( "Insert at the ends~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list( I, List ),
            (99 =:= get( ldir, moveto( endr, insert(endr,99,Z) ) )) and
            (99 =:= get( rdir, moveto( endl, insert(endl,99,Z) ) ))
          end
       )
     end
  ).

% ---------------------------------------------------
% inserts( direction(), [A], yazl(A) ) -> yazl(A).

prop_inserts_nothing() ->
  ?PRINT( "Inserting an empty list is a no-op~n" ),
  ?FORALL( Z, yazls(),
    (Z =:= inserts( rdir, [], Z )) and 
    (Z =:= inserts( ldir, [], Z ))
   ).
 
prop_inserts_empty() ->
  ?PRINT( "Inserting to an empty yazl is just the addition~n" ),
  Add = [x,y],
  (from_list(endl,Add) =:= inserts( rdir, Add, new() )) and 
  (from_list(endr,Add) =:= inserts( ldir, Add, new() )).
  
prop_inserts_list() ->
  ?PRINT( "Inserting a list into a non-empty yazl~n" ),
  List = [a,b,c],
  Add  = [x,y],
  ZL = from_list(endl,List),
  ZR = from_list(endr,List),
  (from_list(endl,Add++List) =:= inserts( rdir, Add, ZL )) and 
  (from_lists(Add,List)      =:= inserts( ldir, Add, ZL )) and
  (from_lists(List,Add)      =:= inserts( rdir, Add, ZR )) and 
  (from_list(endr,List++Add) =:= inserts( ldir, Add, ZR )).

% ---------------------------------------------------
% delete( direction(), yazl(A) ) -> maybe(yazl(A)).

prop_delete_ends() ->
  ?PRINT( "Deleting past the ends does not succeed~n" ),
  ?FORALL( List, list(),
     (endl =:= delete( ldir, from_list(endl,List) )) and
     (endr =:= delete( rdir, from_list(endr,List) ))
  ).

prop_delete_removes_r_element() ->
  ?PRINT( "Delete removes right element~n" ),
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            Z = from_list( I, List ),
            lists:sublist(List,I-1) ++ lists:nthtail(I,List) =:= 
              to_list( delete( rdir, Z ) )
          end
       )
     end
  ).
  
prop_delete_removes_l_element() ->
  ?PRINT( "Delete removes left element~n" ),
  ?FORALL( List, list2(),
     begin
       N = length(List),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list( I, List ),
            lists:sublist(List,I-2) ++ lists:nthtail(I-1,List) =:= 
              to_list( delete( ldir, Z ) )
          end
       )
     end
  ).
  
prop_delete_removes_insert() ->
  ?PRINT( "Delete removes element~n" ),
  ?FORALL( List, list2(),
     begin
       N = length(List),
       ?FORALL( I, range(2,N),
          begin
            Z = from_list( I, List ),
            (Z =:= delete( rdir, insert(rdir,99,Z))) and
            (Z =:= delete( ldir, insert(ldir,99,Z)))
          end
       )
     end
  ).

% ---------------------------------------------------
% truncate( direction(), yazl(A) ) -> yazl(A).

prop_truncate_empty() ->
  ?PRINT( "Truncate empty yazl is no-op~n" ),
  Z = new(),
  (Z =:= truncate(rdir,Z)) and
  (Z =:= truncate(ldir,Z)).

prop_truncate_at_r_end() ->
  ?PRINT( "Truncate at right end is no-op~n" ),
  ?FORALL( List, list1(),
     begin
       Z = from_list(endr,List),
       Z =:= truncate(rdir,Z)
     end
  ).
  
prop_truncate_at_l_end() ->
  ?PRINT( "Truncate at left end is no-op~n" ),
  ?FORALL( List, list1(),
     begin
       Z = from_list(endl,List),
       Z =:= truncate(ldir,Z)
     end
  ).
  
prop_truncate_end_position() ->
  ?PRINT( "Truncate always creates an end position~n" ),
  ?FORALL( Z, nonempty_yazls(),
     (endl =:= position( ldir, truncate(ldir,Z))) and
     (endr =:= position( rdir, truncate(rdir,Z)))
  ).

% ---------------------------------------------------
% reverse( yazl(A) ) -> yazl(A).

prop_reverse_empty() ->
  ?PRINT( "Reversing empty yazl leaves yazl unchanged~n" ),
  new() =:= reverse( new() ).

prop_reverse_single() ->
  ?PRINT( "Reversing a singleton leaves list unchanged~n" ),
  List = [a],
  List =:= to_list( reverse( from_list(List) ) ).

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
  ?FORALL( List, list(),
     (endr =:= position( rdir, reverse( from_list(endl,List) ) )) and
     (endl =:= position( ldir, reverse( from_list(endr,List) ) ))
  ).
  
prop_reverse_switches_current_values() ->
  ?PRINT( "Reversing switches local values~n" ),
  ?FORALL( Z, yazls(),
     begin
       LVal = get( ldir, Z ),
       RVal = get( rdir, Z ),
       RevZ = reverse( Z ),
       (LVal =:= flip( get(rdir,RevZ) )) and 
       (RVal =:= flip( get(ldir,RevZ) ))
     end
  ).
  
prop_reverse_reflects_index() ->
  ?PRINT( "Reversing reflects the index position~n" ),     
  ?FORALL( Z, yazls(),
     begin
       N = size( Z ),
       PosR = position( rdir, Z ),
       PosL = position( ldir, Z ),
       RevZ = reverse( Z ),
       PosRevR = position( rdir, RevZ ),
       PosRevL = position( ldir, RevZ ),
       (PosRevR =:= reflect( rdir, N, PosR )) and
       (PosRevL =:= reflect( ldir, N, PosL ))
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
  ?FORALL( List, list1(),
     begin
       N = length(List),
       ?FORALL( I, range(1,N),
          begin
            F = fun(X) -> 2*X end,
            Z = from_list( I, List ),
            lists:map(F,List) =:= to_list( map(F,Z) )
          end
       )
     end
  ).

% ===================================================
% Performance Tests

-define(   SIZE, 100000 ).
-define( REPEAT,     20 ).

performance_all() ->
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
             fun yazl:moves/3,     [rdir, Len, from_list(List)] ).

perf_fill() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  Array = init_array( Len ),
  perf_run( "Fill - array", 
             fun fill_array/4, [Array, 99, Len, 0] ),
  perf_run( "Fill -  yazl", 
             fun fill_yazl/3,  [rdir, 99, from_list(List)] ).

perf_insert() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  perf_run( "Insert - yazl", fun inflate/3, 
             [rdir,Len-1,from_list(List)] ).

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
             [rdir,  ?SIZE, from_list(List)] ).
             
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