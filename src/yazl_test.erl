
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
% new() -> empty_yazl().

prop_new_value() ->
  equals( new(), {[],[]} ).

% ---------------------------------------------------
% from_list( [A] ) -> yazl(A).

prop_from_list_to_list() ->
  ?FORALL( {L,I}, {list(),integer()},
     ?IMPLIES( 
        (1 =< I) andalso (I =< length(L)),
        L =:= yazl:to_list( yazl:from_list(I,L) )
     )
  ).
  
% ---------------------------------------------------
% from_list( position(), [A] ) -> yazl(A).

prop_from_list_i_to_list() ->
  ?FORALL( L, list(), 
           L =:= yazl:to_list( yazl:from_list(L) )
  ).
  
% ---------------------------------------------------
% is_yazl( term() ) -> boolean().

% ---------------------------------------------------
% is_empty( yazl(_) ) -> boolean().

% ---------------------------------------------------
% size( yazl(_) ) -> non_neg_integer().

prop_size_list() ->
  ?FORALL( L, list(), 
           length(L) =:= yazl:size( yazl:from_list(L) )
  ).

% ---------------------------------------------------
% position( direction(), yazl(_) ) -> position().

% ---------------------------------------------------
% to_list( yazl(A) ) -> [A].

% ---------------------------------------------------
% get( direction(), yazl(A) ) -> maybe(A).

% ---------------------------------------------------
% move( direction(), yazl(A) ) -> maybe(yazl(A)).

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
% reverse( yazl(A) ) -> yazl(A).

% ---------------------------------------------------
% map( direction(), fun((A)->B), yazl(A) ) -> [B].

% ---------------------------------------------------
% foldl( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

% ---------------------------------------------------
% foldr( direction(), fun((A,B)->B), B, yazl(A) ) -> B.

%====================================================================

