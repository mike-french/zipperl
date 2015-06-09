%% coding: utf-8

%--------------------------------------------------------------------
%
% Copyright (c) 2015 Mike French
%
% This software is released under the MIT license
% http://www.opensource.org/licenses/mit-license.php
%
%--------------------------------------------------------------------

% @doc YAZL '<i>yazzle</i>' (Yet Another Zipper List): 
% A mutable list with a current focus position.
%
% A yazl supports operations normally found in
% mutable doubly-linked lists, such as read, update,
% insert, delete and incremental bi-directional traversal.
% Local operations in the neighbourhood of the focus
% are executed in O(1) constant time.
% The yazl also provides global operations and index-based
% random access, typically with an O(n) performance penalty.
%
% The focus may be between two elements of the list, or at one of the ends. 
% The descriptions used here are slightly different from a true zipper,
% because the focus is between elements, not at a current element
% [<i>Functional Pearl: The Zipper</i>, Gérard Huet, 1997].
%
% We describe lists as being ordered left-to-right,
% like western writing, with the excuse that this bias is already
% present in Erlang with the names <i>foldl</i>, <i>foldr</i>.
%
% The position of the current element is either a 1-based positive integer,
% or an end marker: `endl', for the beginning, or `endr' for the end.
% The current value is after (to the right of) the focus, if it exists. 
% There is no current value for empty lists,
% or non-empty lists with the focus after the right end.
%
% Functions on single values and lists of values are not overloaded,
% they are given distinct names (<i>e.g.</i>`insert'/`inserts'),
% so that yazls can have lists as regular elements 
% (<i>i.e.</i> lists of lists).
%
% == Usage ==
%
% === Create, Import, Export ===
%
% Create yazls from lists using `new' and `from_list'.
% Test if a term appears to be a yazl with `is_yazl'.
% Recover the underlying list with `to_list'.
%
% === Query ===
%
% Test if the yazl is empty with `is_empty'.
% Get the total length of the underlying list using `size'.
% Read the value at the current focus position using `get'.
% Find the current focus location using `position',
% which may return a 1-based integer index, or an ending marker.
%
% === Move ===
%
% Movement functions change the focus position,
% but do not change the content of the list.
% Movements return special flags `endl' or `endr' 
% if an operation would take the focus
% beyond the beginning or end of the list.
% Client code can implement cyclic behaviour by using
% these flags in conjunction with the `moveto' function.
%
% Move the focus with `move', `moves', `moveto'. 
% The `move' function changes focus to the next or previous elements. 
% The `moves' function jumps multiple steps relative to the current focus.
% The `moveto' function jump to absolute positions based on
% a specific index, or the beginning or end of the list.
%
% === Search ===
%
% Move the focus by searching with `find', `finds',
% `moveuntil' and `movewhile'.
% The `find' function will search for the next or previous 
% occurrence of a value. The `finds' function searches for the 
% next or previous occurrence of a sequence of values. 
% The `moveuntil' (`movewhile') functions search until a 
% boolean predicate function of the current value becomes true (false).
%
% === Update ===
%
% Write the value at the current focus position using `set'.
%
% Add new values on either side of the current focus,
% or at the head or tail of the underlying list, using
% `insert' and `inserts'.
%
% Delete the element at the current focus position using `delete'.
% Delete from the focus to one of the ends using the `truncate'.
%
% Reverse the whole list while keeping the same focus element
% using `reverse' - note this is constant time O(1).
%
% === Partial Function Application ===
%
% Apply a partial map function on one side of the focus using <i>map</i>.
% Apply a partial fold on one side of the focus
% using <i>foldl</i> and <i>foldr</i> functions.
%
% == Efficiency ==
%
% The implementation is efficient (constant time)
% for local operations at the focus: <br/>
% `new, from_list/1, move, get, set, insert, inserts, 
%  delete, reverse, truncate'.
%
% Incremental operations will incur a cost proportional
% to the distance from the focus to the target position:<br/>
% `from_list/2, moves, moveto, moveuntil, 
% find, finds'.
%
% Global operations will incur a cost proportional to the
% length of the underlying list: <br/>
% `to_list, size, position'.
%
% Maps from the focus are as efficient as <i>map</i> on lists.
% All folds are implemented in O(n) with <i>foldl</i>; some 
% directions (`foldr/r', `foldl/l') also require an O(n) 
% list reverse.

-module('yazl').

% ===================================================
% API and type exports

-export( [
   delete/2,
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
         
-export_types( [
   yazl/1,
   empty_yazl/0,
   direction/0,
   ending/0,
   index/0,
   maybe/1,
   position/0,
   predicate/1 
] ).

% ===================================================
% Types

% A yazl is a tuple of two lists.
-type yazl(A) :: { [A], [A] }.

% An empty yazl is just two empty lists.
-type empty_yazl() :: { [], [] }.

% Directions are rightward and leftward.
-type direction() :: r | l.

% A position off the beginning or end of the list.
-type ending() :: endr | endl.

% A 1-based index of a position in the list.
% The value will be between 1 and `size', inclusive.
-type index() :: pos_integer().

% Expand a generic type to include the two endings.
-type maybe(A) :: ending() | A.

% A position for the focus at an index or at the ends.
-type position() :: maybe( index() ).

% A predicate function used for filtering and searching.
-type predicate(A) :: fun( (A)->boolean() ).

% ===================================================
% Type utilities

% ---------------------------------------------------
% @doc Type utility: get the opposite of a direction.

-spec opposite( direction() ) -> direction().

opposite( r ) -> l;
opposite( l ) -> r.

% ---------------------------------------------------
% @doc Type utility: Get the end in a specific direction.

-spec ending( direction() ) -> ending().

ending( r ) -> endr;
ending( l ) -> endl.

% ==============================================================
% Constructors

% ---------------------------------------------------
% @doc Constructor: create a new empty yazl.

-spec new() -> empty_yazl().

new() -> { [], [] }.

% ---------------------------------------------------
% @doc Constructor: create a yazl with focus before the first element 
% of a list. If the list is empty, the empty yazl is returned.

-spec from_list( [A] ) -> yazl(A).

from_list( List ) when is_list(List) -> { [], List }.

% ---------------------------------------------------
% @doc Constructor: create a yazl with focus at the 
% beginning, at the end, or before the Ith element of a list.
% The index is 1-based, so the first element is 1,
% and the last index is equal to the length of the list.
% To position at the beginning of the list, pass `endl'.
% To position at the end of the list, pass `endr'.
% It is an error to pass an integer less than 1,
% or greater than the length of the list,
% so passing 1 with the empty list is an error.
% If the list is empty, the empty yazl is returned.

-spec from_list( position(), [A] ) -> yazl(A).

from_list( endl, List ) when is_list(List) -> 
  { [], List };
from_list( endr, List ) when is_list(List) ->
  { lists:reverse(List), [] };
from_list(    I, List ) when is_list(List) 
  and is_integer(I) and (I >= 1) and (I =< length(List)) -> 
  { L, R } = lists:split( I-1, List ),
  { lists:reverse(L), R }.

% ==============================================================
% Queries

% ---------------------------------------------------
% @doc Test if a term appears to be a yazl.

-spec is_yazl( term() ) -> boolean().

is_yazl( {L,R} ) -> is_list(L) andalso is_list(R);
is_yazl( _ )     -> false.

% ---------------------------------------------------
% @doc Test if a yazl is empty.

-spec is_empty( yazl(_) ) -> boolean().

is_empty( {[],[]} ) -> true;
is_empty( { _,_ } ) -> false.

% ---------------------------------------------------
% @doc Get the length of the underlying list.
% If the yazl is empty, the size is 0.

-spec size( yazl(_) ) -> non_neg_integer().

size( {L,R} ) -> length(L) + length(R).

% ---------------------------------------------------
% @doc Get the one-based index of the position to
% the right or left of the current focus.
% Indices are 1-based.
% If the yazl is empty, or focus is at the beginning of 
% a non-empty list, then the left index is `endl'.
% If the yazl is at the end of a non-empty list, 
% then the right index is `endr'.
%
% Note that having to return a position for the empty
% yazl breaks the symmetry. 
% The return value will be `endl', 
% even though it is also `endr' for rightward operations.

-spec position( direction(), yazl(_) ) -> position().

position( l, {[],_ } ) -> endl;
position( r, { _,[]} ) -> endr;
position( l, { L,_ } ) -> length(L);
position( r, { L,_ } ) -> length(L) + 1.


% ---------------------------------------------------
% @doc Recover the underlying list.
% If the yazl is empty, the result is the empty list.

-spec to_list( yazl(A) ) -> [A].

to_list( {[],[]} ) -> [];
to_list( { L,R } ) -> lists:reverse( L, R ).

% ---------------------------------------------------
% @doc Get the value of the element to the right or
% left of the current focus.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.

-spec get( direction(), yazl(A) ) -> maybe(A).

get( r, { _,[]} ) -> endr;
get( l, {[],_ } ) -> endl;
get( r, {_,[H|_]} ) -> H;
get( l, {[H|_],_} ) -> H.

% ==============================================================
% Move focus

% ---------------------------------------------------
% @doc Move the focus one step to the right or left.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.
%
% Traditional function `next(...)', 
% is equivalent to the curried form `move( r, ... )'.
% Traditional function `prev(...)', 
% is equivalent to the curried form `move( l, ... )'.

-spec move( direction(), yazl(A) ) -> maybe(yazl(A)).

move( r, { _,[]} ) -> endr;
move( l, {[], _} ) -> endl;
move( r, { L,[RH|RT]} ) -> { [RH|L], RT };
move( l, {[HL|TL],R } ) -> { TL, [HL|R] }.

% ---------------------------------------------------
% @doc Move the focus multiple steps to the right or left.
% If the yazl is empty, or the steps would
% overrun the beginning or end of the list, 
% then return `endr' or `endl'.
%
% Moving a zero offset leaves the yazl unchanged.
%
% Negative offsets are converted to the equivalent positive
% offset in the other direction, which may return an
% unexpected opposite end value,
% e.g. moves(r,-2,Z) may return `endl'.

-spec moves( direction(), integer(), yazl(A) ) -> maybe(yazl(A)).

moves( _, 0,     Z ) -> Z;
moves( D, 1,     Z ) -> move( D, Z );
moves( D, I,     Z ) when (I < 0) -> moves( opposite(D), -I, Z );
moves( r, I, {_,R} ) when (I > length(R)) -> endr;
moves( l, I, {L,_} ) when (I > length(L)) -> endl;
moves( r, I, {L,R} ) -> { RH, RT } = lists:split( I, R ),
                        { lists:reverse(RH,L), RT };
moves( l, I, {L,R} ) -> { LH, LT } = lists:split( I, L ),
                        { LT, lists:reverse(LH,R) }.

% ---------------------------------------------------
% @doc Move to the beginning or end of the list,
% or an absolute index position within the list.
% The position is `endr' or `endl',
% or an integer signifying a index,
% <i>i.e.</i> focus before the given index.
% If the index offset would overrun the beginning
% or end of the list, then return `endr' or `endl'.

-spec moveto( position(), yazl(A) ) -> maybe(yazl(A)).

moveto( endr, Z={ _,[]} ) -> Z;
moveto( endl, Z={[],_ } ) -> Z;
moveto( endr,   { L,R } ) -> { lists:reverse(R,L), [] };
moveto( endl,   { L,R } ) -> { [], lists:reverse(L,R) };
moveto( I,    Z={ _,_ } ) when is_integer(I) ->
  Len = yazl:size( Z ),
  IR  = case position( r, Z ) of 
          endr   -> Len+1; 
          IndexR -> IndexR 
        end,
  case I of
    I when (I <  1  ) -> endl;
    I when (I >  Len) -> endr;
    I when (I == IR ) -> Z;
    I when (I <  IR ) -> moves( l, IR-I, Z );
    I when (I >  IR ) -> moves( r, I-IR, Z )
  end.
  
% ---------------------------------------------------
% @doc Search for the first occurrence of a value. 
% If the search is successful, return a yazl that
% focuses before (to the left) of the found element.
% If the search does not find the value,
% then it returns `endr' or `endl'.

-spec find( direction(), A, yazl(A) ) -> maybe(yazl(A)).

find( r, _, { _,[]} ) -> endr;
find( l, _, {[],_ } ) -> endl;
find( r, Val, Z={ _,[Val|_] } ) -> Z;
find( l, Val, Z={ [Val|_],_ } ) -> move(l,Z); 
find( D, Val, Z ) -> find( D, Val, move(D,Z) ).

% ---------------------------------------------------
% @doc Search for the first sequence of values  
% that match a given non-empty list.
% The target sequence always runs left-to-right,
% even if the search is to the left (backwards).
% If the search is successful, it returns a yazl that
% focuses before the beginning (to the left) of the sequence.
% If the search does not find the value,
% then it returns `endr' or `endl'.
%
% A search for an empty list is a no-op
% that returns the original yazl
% (following the convention of `lists:prefix' 
%  that the empty list is a prefix of all lists).

-spec finds( direction(), [A], yazl(A) ) -> maybe(yazl(A)).

finds( r,        [], Z ) -> Z;
finds( r, Vs=[V|VT], Z ) ->
  case find(r,V,Z) of
    endr -> endr;
    Y={ _, [V|RT] } -> 
      case lists:prefix(VT,RT) of
        true  -> Y;
        false -> finds( r, Vs, move(r,Y) ) 
      end
  end;
finds( l, Vs, Z ) -> 
  case finds( r, lists:reverse(Vs), reverse(Z) ) of
    endr -> endl;
    Y    -> reverse( moves( r, length(Vs), Y ) )
  end.
  
% ---------------------------------------------------
% @doc Search for the first occurrence of a value
% that satisfies a boolean predicate function.
% If the search is successful, it returns a yazl 
% that focuses before the found element.
% If the search does not find the value,
% then it returns `endr' or `endl'.
% Note this is equivalent to `movewhile' 
% using the negation of the predicate. 

-spec moveuntil( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

moveuntil( r, _, { _,[]} ) -> endr;
moveuntil( l, _, {[],_ } ) -> endl;
moveuntil( r, Pred, Z={_,[RH|_]} ) ->
  case Pred(RH) of
    true  -> Z;
    false -> moveuntil( r, Pred, move(r,Z) )
  end;
moveuntil( l, Pred, Z={[LH|_],_} ) ->
  case Pred(LH) of
    true  -> move( l, Z );
    false -> moveuntil( l, Pred, move(l,Z) )
  end.
  
% ---------------------------------------------------
% @doc Search for the first occurrence of a value
% that does not satisfy a boolean predicate function.
% If the search is successful, it returns a yazl that
% focuses before the found element.
% If the search does not find the value,
% then it returns `endr' or `endl'.
% Note this is equivalent to `moveuntil' 
% using the negation of the predicate. 

-spec movewhile( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

movewhile( D, Pred, Z ) -> 
  moveuntil( D, fun(A) -> not Pred(A) end, Z ).

% ==============================================================
% Update

% ---------------------------------------------------
% @doc Set the value of the element to the right or
% left of the current focus.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.

-spec set( direction(), A, yazl(A) ) -> maybe(yazl(A)).

set( r, _, { _,[]} ) -> endr;
set( l, _, {[], _} ) -> endl;
set( r, V, {L,[_|RT]} ) -> { L,[V|RT]};
set( l, V, {[_|LT],R} ) -> {[V|LT],R }.

% ---------------------------------------------------
% @doc Insert a value to the right or left of the current focus,
% or at the beginning (prepend) or end (append) of the whole list.
% Whether it is to the left or right 
% does not affect the final content of the list, 
% just the final position of the focus
% relative to the inserted sequence.

-spec insert( direction() | ending(), A, yazl(A) ) -> yazl(A).

insert(    r, V, {L,R} ) -> {     L , [V|R] };
insert(    l, V, {L,R} ) -> {  [V|L],    R  };
insert( endl, V, {L,R} ) -> { L++[V], R      };
insert( endr, V, {L,R} ) -> { L,      R++[V] }.

% ---------------------------------------------------
% @doc Insert a sequence of values to the left or right
% of the current focus, or at the beginning (prepend) 
% or end (append) of the whole list. 
% Whether it is inserted to the left or right 
% does not affect the final content of the list, 
% just the final position of the focus
% relative to the inserted sequence.
% Inserting an empty sequence does not change the underlying list.

-spec inserts( direction(), [A], yazl(A) ) -> yazl(A).

inserts(    _, [],Z={_,_} ) -> Z;
inserts(    r, Vs,  {L,R} ) -> { L,                    Vs++R };
inserts(    l, Vs,  {L,R} ) -> { lists:reverse(Vs,L),      R };
inserts( endr, Vs,  {L,R} ) -> { L,                    R++Vs };
inserts( endl, Vs,  {L,R} ) -> { L++lists:reverse(Vs), R     }.

% ---------------------------------------------------
% @doc Delete the value to the right or left of the focus.
% If the yazl is empty, or the focus is already
% at the beginning or end of a list, then return `endr' or `endl'.

-spec delete( direction(), yazl(A) ) -> maybe(yazl(A)).

delete( r, { _,[]} ) -> endr;
delete( l, {[],_ } ) -> endl;
delete( r, {L,[_|RT]} ) -> { L,RT};
delete( l, {[_|LT],R} ) -> {LT,R }.

% ---------------------------------------------------
% @doc Delete the indicated sublist.
% If the yazl is empty, return the empty yazl.
% For right, the focus will be positioned after the 
% last element of the left sublist.
% For left, the focus will be positioned before the 
% first element of the right sublist.

-spec truncate( direction(), yazl(A) ) -> yazl(A).

truncate( r, {L,_} ) -> { L,[]};
truncate( l, {_,R} ) -> {[],R }.

% ---------------------------------------------------
% @doc Reverse the list maintaining focus.
% If the yazl is empty, the result is the empty yazl.
% This is very fast O(1), compared to O(n) for ordinary list.

-spec reverse( yazl(A) ) -> yazl(A).

reverse( {L,R} ) -> {R,L}.

% ==============================================================
% Partial Function Application

% ---------------------------------------------------
% @doc Apply a map from the current focus to the left or right sublist.
% If the yazl or its sublist is empty, return the empty list.

-spec map( direction(), fun((A)->B), yazl(A) ) -> [B].

map( r, Fun, {_,R} ) -> lists:map( Fun, R );
map( l, Fun, {L,_} ) -> lists:map( Fun, lists:reverse(L) ).

% ---------------------------------------------------
% @doc Apply a foldl from the current focus to the left or right sublist.
% If the yazl or its sublist is empty, 
% return the initial value of the accumulator.

-spec foldl( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

foldl( r, Fun, Init, {_,R} ) -> lists:foldl( Fun, Init, R );
foldl( l, Fun, Init, {L,_} ) -> lists:foldl( Fun, Init, lists:reverse(L) ).

% ---------------------------------------------------
% @doc Apply a foldr from the current focus to the left or right sublist.
% If the yazl or its sublist is empty, 
% return the initial value of the accumulator.

-spec foldr( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

foldr( r, Fun, Init, {_,R} ) -> lists:foldl( Fun, Init, lists:reverse(R) );
foldr( l, Fun, Init, {L,_} ) -> lists:foldl( Fun, Init, L ).

%====================================================================
