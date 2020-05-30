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
% Create yazls from lists using `new', `from_list' and `from_lists'.
% Test if a term appears to be a yazl with `is_yazl'.
% Recover the underlying list with `to_list'.
%
% === Query ===
%
% Test if the yazl is empty with `is_empty'.
% Get the total length of the underlying list using `size'.
% Read the value at the current focus position using `get' or `gets'.
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
% Reverse the whole list while keeping the same focus
% using `reverse' - note this is constant time O(1).
%
% === Function Application ===
%
% Apply a <i>map</i> function while leaving the focus unchanged.
%
% == Efficiency ==
%
% The implementation is efficient constant time, O(1):
% for local operations at the focus: <br/>
% `new, from_list/1, move, get, set, insert,
%  delete, reverse, truncate'.
%
% Incremental operations will incur a cost proportional
% to the distance from the focus to the target position:<br/>
% `from_list/2, from_lists, gets, sets, moves, moveto, moveuntil,
% find, finds, inserts'.
%
% Global operations will incur a cost proportional to the
% length of the underlying list O(n): <br/>
% `to_list, size, position'.

-module('yazl').

% ===================================================
% API and type exports

-export( [
   delete/2,
   ending/1,
   find/3,
   finds/3,
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

% Directions are to the left (beginning) or to the right (end) of the list.
-type direction() :: ldir | rdir.

% A position before the left (beginning) or past the right (end) of the list.
-type ending() :: endl | endr.

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

opposite( rdir ) -> ldir;
opposite( ldir ) -> rdir.

% ---------------------------------------------------
% @doc Type utility: get the end in a specific direction.

-spec ending( direction() ) -> ending().

ending( rdir ) -> endr;
ending( ldir ) -> endl.

% ==============================================================
% Constructors

% ---------------------------------------------------
% @doc Constructor: create a new empty yazl.

-spec new() -> empty_yazl().

new() -> { [], [] }.

% ---------------------------------------------------
% @doc Constructor: create a yazl with focus before the first element
% of a list. If the list is empty, the empty yazl is returned.
% Equivalent to calling `from_list/2' with position argument `endl'.

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
% The position for the index is implicitly to the right,
% so for a non-empty list,
% passing `endl' is the same as passing 1.

-spec from_list( position(), [A] ) -> yazl(A).

from_list( endl, List ) when is_list(List) ->
  { [], List };
from_list( endr, List ) when is_list(List) ->
  { lists:reverse(List), [] };
from_list(    I, List ) when is_list(List) and is_integer(I)
                         and (I >= 1) and (I =< length(List)) ->
  { L, R } = lists:split( I-1, List ),
  { lists:reverse(L), R }.

% ---------------------------------------------------
% @doc Constructor: create a yazl with focus between two sublists.
% The underlying list will be the concatenation of the two lists.
% The focus will be after (right of) the last element of the first list,
% and before (left of) the first element of the second list.
% If both lists are empty, the empty yazl is returned.

-spec from_lists( [A], [A] ) -> yazl(A).

from_lists( L, R ) when is_list(L) and is_list(R) ->
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
% The performance is O(n).

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
% The performance is proportional to the position in the list.
% If the focus is at `endl' it is O(1),
% but if the focus is at the last element, it is O(n).

-spec position( direction(), yazl(_) ) -> position().

position( ldir, {[],_ } ) -> endl;
position( rdir, { _,[]} ) -> endr;
position( ldir, { L,_ } ) -> length(L);
position( rdir, { L,_ } ) -> length(L) + 1.


% ---------------------------------------------------
% @doc Recover the underlying list.
% If the yazl is empty, the result is the empty list.
% The cost is proportional to the position in the list.
% If the focus is at `endl' it is O(1),
% but if the focus is at `endr' it is O(n).

-spec to_list( yazl(A) ) -> [A].

to_list( {[],[]} ) -> [];
to_list( { L,R } ) -> lists:reverse( L, R ).

% ---------------------------------------------------
% @doc Recover the underlying sublists before and after the focus.
% If the yazl is empty, the result is two empty lists.
% The underlying list is equal to the concatenation of the two lists.
% The cost is proportional to the position in the list.
% If the focus is at `endl' it is O(1),
% but if the focus is at `endr' it is O(n).

-spec to_lists( yazl(A) ) -> { [A], [A] }.

to_lists( Z={[],[]} ) -> Z;
to_lists(  { L, R } ) -> { lists:reverse(L), R }.

% ---------------------------------------------------
% @doc Get the value of the element to the right or
% left of the current focus.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.
% This is fast constant time O(1).

-spec get( direction(), yazl(A) ) -> maybe(A).

get( rdir, { _,[]} ) -> endr;
get( ldir, {[],_ } ) -> endl;
get( rdir, {_,[H|_]} ) -> H;
get( ldir, {[H|_],_} ) -> H.

% ---------------------------------------------------
% @doc Get the values of elements to the right or
% left of the current focus.
% Getting zero elements returns the empty list.
% Getting a negative number of elements,
% returns elements from the other direction.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.
% Performance is proportional to the length of the requested sublist.

-spec gets( direction(), integer(), yazl(A) ) -> [A].

gets(    _, 0, _     ) -> [];
gets(  Dir, N, Z     ) when (N < 0) -> gets( opposite(Dir), -N, Z );
gets( rdir, N, {_,R} ) when (N > length(R)) -> endr;
gets( ldir, N, {L,_} ) when (N > length(L)) -> endl;
gets(  Dir, 1, Z     ) -> [get( Dir, Z )];
gets( rdir, N, {_,R} ) -> lists:sublist(R,N);
gets( ldir, N, {L,_} ) -> lists:reverse( lists:sublist(L,N) ).

% ==============================================================
% Move focus

% ---------------------------------------------------
% @doc Move the focus one step to the right or left.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.
%
% Traditional function `next(...)',
% is equivalent to the curried form `move( rdir, ... )'.
% Traditional function `prev(...)',
% is equivalent to the curried form `move( ldir, ... )'.
% This is fast constant time O(1).

-spec move( direction(), yazl(A) ) -> maybe(yazl(A)).

move( rdir, { _,[]} ) -> endr;
move( ldir, {[], _} ) -> endl;
move( rdir, { L,[RH|RT]} ) -> { [RH|L], RT };
move( ldir, {[HL|TL],R } ) -> { TL, [HL|R] }.

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
% e.g. `moves(rdir,-2,Z)' may return `endl'.

-spec moves( direction(), integer(), yazl(A) ) -> maybe(yazl(A)).

moves(    _, 0,  Z    ) -> Z;
moves(  Dir, 1,  Z    ) -> move( Dir, Z );
moves(  Dir, I,  Z    ) when (I < 0) -> moves( opposite(Dir), -I, Z );
moves( rdir, I, {_,R} ) when (I > length(R)) -> endr;
moves( ldir, I, {L,_} ) when (I > length(L)) -> endl;
moves( rdir, I, {L,R} ) ->
  { RH, RT } = lists:split( I, R ),
  { lists:reverse(RH,L), RT };
moves( ldir, I, {L,R} ) ->
  { LH, LT } = lists:split( I, L ),
  { LT, lists:reverse(LH,R) }.

% ---------------------------------------------------
% @doc Move to the beginning or end of the list,
% or an absolute index position within the list.
% The position is `endr' or `endl',
% or a 1-based integer signifying a index,
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
  IR  = case position(rdir,Z) of
          endr   -> Len+1;
          IndexR -> IndexR
        end,
  case I of
    I when (I <  1  ) -> endl;
    I when (I >  Len) -> endr;
    I when (I == IR ) -> Z;
    I when (I <  IR ) -> moves( ldir, IR-I, Z );
    I when (I >  IR ) -> moves( rdir, I-IR, Z )
  end.

% ---------------------------------------------------
% @doc Search for the first occurrence of a value.
% If the search is successful, return a yazl that
% focuses before (right search) or after (left search)
% the found element.
% If the search does not find the value,
% then it returns `endr' or `endl'.

-spec find( direction(), A, yazl(A) ) -> maybe(yazl(A)).

find( rdir, _, { _,[]} ) -> endr;
find( ldir, _, {[],_ } ) -> endl;
find( rdir, Val, Z={ _,[Val|_] } ) -> Z;
find( ldir, Val, Z={ [Val|_],_ } ) -> Z;
find(  Dir, Val, Z ) -> find( Dir, Val, move(Dir,Z) ).

% ---------------------------------------------------
% @doc Search for the first sequence of values
% that match a given non-empty list.
% If the search is successful, return a yazl that
% focuses before (right search) or after (left search)
% the found list of elements.
% If the search does not find the value,
% then it returns `endr' or `endl'.
%
% A search for an empty list is a no-op
% that returns the original yazl
% (following the convention of `lists:prefix'
%  that the empty list is a prefix of all lists).

-spec finds( direction(), [A], yazl(A) ) -> maybe(yazl(A)).

finds( _,           [], Z ) -> Z;
finds( rdir, Vs=[V|VT], Z ) ->
  case find(rdir,V,Z) of
    endr -> endr;
    Y={ _, [V|RT] } ->
      case lists:prefix(VT,RT) of
        true  -> Y;
        false -> finds( rdir, Vs, move(rdir,Y) )
      end
  end;
finds( ldir, Vs, Z ) ->
  case finds( rdir, lists:reverse(Vs), reverse(Z) ) of
    endr -> endl;
    Y    -> reverse( Y )
  end.

% ---------------------------------------------------
% @doc Search for the first occurrence of a value
% that satisfies a boolean predicate function.
% If the search is successful, it returns a yazl
% that focuses before the found element.
% If the search does not find the value,
% then it returns `endr' or `endl'.
%
% Note this is equivalent to `movewhile'
% using the negation of the predicate.

-spec moveuntil( direction(), predicate(A), yazl(A) ) -> maybe(yazl(A)).

moveuntil( rdir, _, { _,[]} ) -> endr;
moveuntil( ldir, _, {[],_ } ) -> endl;
moveuntil( rdir, Pred, Z={_,[RH|_]} ) ->
  case Pred(RH) of
    true  -> Z;
    false -> moveuntil( rdir, Pred, move(rdir,Z) )
  end;
moveuntil( ldir, Pred, Z={[LH|_],_} ) ->
  case Pred(LH) of
    true  -> Z;
    false -> moveuntil( ldir, Pred, move(ldir,Z) )
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

movewhile( Dir, Pred, Z ) ->
  moveuntil( Dir, fun(A) -> not Pred(A) end, Z ).

% ==============================================================
% Update

% ---------------------------------------------------
% @doc Set the value of the element to the right or
% left of the current focus.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.
% This is fast constant time O(1).

-spec set( direction(), A, yazl(A) ) -> maybe(yazl(A)).

set( rdir, _, { _,[]} ) -> endr;
set( ldir, _, {[], _} ) -> endl;
set( rdir, V, {L,[_|RT]} ) -> { L,[V|RT]};
set( ldir, V, {[_|LT],R} ) -> {[V|LT],R }.

% ---------------------------------------------------
% @doc Set values of elements to the right or
% left of the current focus.
% Setting the empty list is a no-op,
% and returns the original yazl.
% If the operation would overrun the begining or end
% of the list, return `endr' or `endl'.
% Performance is proportional to the length of the requested sublist.

-spec sets( direction(), [A], yazl(A) ) -> maybe(yazl(A)).

sets( ldir, [],  Z    ) -> Z;
sets( rdir, Vs, {_,R} ) when (length(Vs) > length(R)) -> endr;
sets( ldir, Vs, {L,_} ) when (length(Vs) > length(L)) -> endl;
sets(  Dir, [V], Z    ) -> set( Dir, V, Z );
sets( rdir, Xs, {L,R} ) -> { L,
                             Xs++lists:nthtail(length(Xs),R) };
sets( ldir, Xs, {L,R} ) -> { lists:reverse(Xs)++
                             lists:nthtail(length(Xs),L), R }.

% ---------------------------------------------------
% @doc Insert a value to the right or left of the current focus,
% or at the beginning (prepend) or end (append) of the whole list.
% Whether it is to the left or right
% does not affect the final content of the list,
% just the final position of the focus
% relative to the inserted sequence.
% This is fast constant time O(1).

-spec insert( direction() | ending(), A, yazl(A) ) -> yazl(A).

insert( rdir, V, {L,R} ) -> {     L ,  [V|R] };
insert( ldir, V, {L,R} ) -> {  [V|L],     R  };
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

-spec inserts( direction() | ending(), [A], yazl(A) ) -> yazl(A).

inserts(    _, [],Z={_,_} ) ->   Z;
inserts( rdir, Vs,  {L,R} ) -> { L,                    Vs++R };
inserts( ldir, Vs,  {L,R} ) -> { lists:reverse(Vs,L),      R };
inserts( endr, Vs,  {L,R} ) -> { L,                    R++Vs };
inserts( endl, Vs,  {L,R} ) -> { L++lists:reverse(Vs), R     }.

% ---------------------------------------------------
% @doc Delete the value to the right or left of the focus.
% If the yazl is empty, or the focus is already
% at the beginning or end of a list, then return `endr' or `endl'.
% This is fast constant time O(1).

-spec delete( direction(), yazl(A) ) -> maybe(yazl(A)).

delete( rdir, { _,[]} ) -> endr;
delete( ldir, {[],_ } ) -> endl;
delete( rdir, {L,[_|RT]} ) -> { L,RT};
delete( ldir, {[_|LT],R} ) -> {LT,R }.

% ---------------------------------------------------
% @doc Delete the indicated sublist.
% If the yazl is empty, return the empty yazl.
% For right, the focus will be positioned after the
% last element of the left sublist.
% For left, the focus will be positioned before the
% first element of the right sublist.
% This is fast constant time O(1).

-spec truncate( direction(), yazl(A) ) -> yazl(A).

truncate( rdir, {L,_} ) -> { L,[]};
truncate( ldir, {_,R} ) -> {[],R }.

% ---------------------------------------------------
% @doc Reverse the list maintaining focus.
% If the yazl is empty, the result is the empty yazl.
% If the yazl is not empty, the current values to the
% right and left will be switched
% This is fast constant time O(1),
% compared to O(n) for ordinary list.

-spec reverse( yazl(A) ) -> yazl(A).

reverse( {L,R} ) -> {R,L}.

% ==============================================================
% Partial Function Application

% ---------------------------------------------------
% @doc Apply a map while leaving the focus unchanged.
% If the yazl is empty it will be unchanged.

-spec map( fun((A)->B), yazl(A) ) -> yazl(B).

map( Fun, {L,R} ) -> { lists:map(Fun,L), lists:map(Fun,R) }.

%====================================================================
