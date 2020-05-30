%--------------------------------------------------------------------
%
% Copyright (c) 2015 Mike French
%
% This software is released under the MIT license
% http://www.opensource.org/licenses/mit-license.php
%
%--------------------------------------------------------------------

-module( yazl_perf_tests ).

-compile( {no_auto_import,[size/1]} ).

-export( [performance_all/0] ).

% ---------------------------------------------------
% Libraries

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

% ---------------------------------------------------
% Debug macros to tunnel through EUnit stdout capture

-define( PRINT(MSG),       io:format( user, MSG, []   ) ).
-define( FORMAT(FMT,ARGS), io:format( user, FMT, ARGS ) ).

% ===================================================
% Performance Tests

-define(   SIZE, 100000 ).
-define( REPEAT,     20 ).

% ---------------------------------------------------

perf_test_() ->
  {timeout, 300, fun performance_all/0 }.

performance_all() ->
  delim(),
  ?FORMAT( "Performance Tests:  Size ~p   Repeat ~p~n", [?SIZE,?REPEAT] ),
  delim(),
  perf_create(), delim(),
  perf_move(),   delim(),
  perf_fill(),   delim(),
  perf_insert(), delim(),
  perf_map(),    delim(),
  perf_find(),   delim().

% ---------------------------------------------------

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
             fun yazl:moves/3,     [rdir, Len, yazl:from_list(List)] ).

perf_fill() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  Array = init_array( Len ),
  perf_run( "Fill - array",
             fun fill_array/4, [Array, 99, Len, 0] ),
  perf_run( "Fill -  yazl",
             fun fill_yazl/3,  [rdir, 99, yazl:from_list(List)] ).

perf_insert() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  perf_run( "Insert - yazl", fun inflate/3,
             [rdir,Len-1,yazl:from_list(List)] ).

perf_map() ->
  Len     = ?SIZE,
  List    = lists:seq( 1, Len ),
  Array   = init_array( Len ),
  Double  = fun(     X ) -> 2*X end,
  IDouble = fun( _I, X ) -> 2*X end,
  perf_run( "Map - array", fun array:map/2,
             [IDouble, Array] ),
  perf_run( "Map -  yazl", fun yazl:map/2,
             [Double, yazl:from_list(List)] ).

perf_find() ->
  Len   = ?SIZE,
  List  = lists:seq( 1, Len ),
  Array = init_array( Len ),
  perf_run( "Find - array", fun find_array/4,
             [Array, ?SIZE, Len, 0] ),
  perf_run( "Find -  yazl", fun yazl:find/3,
             [rdir,  ?SIZE, yazl:from_list(List)] ).

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
  yazl:set( Dir, V, Z ),
  case yazl:move( Dir, Z ) of
    endr -> endr;
    Z1   -> fill_yazl( Dir, V, Z1 )
  end.

inflate( Dir, V, Z ) ->
  Z1 = yazl:insert( yazl:opposite(Dir), V, Z ),
  case yazl:move( Dir, Z1 ) of
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
        Now1 = erlang:timestamp(),
        erlang:apply( Fun, Args ),
        Now2 = erlang:timestamp(),
        { memory, Mem2 } = erlang:process_info( self(), memory ),
        { Elapsed+timer:now_diff(Now2,Now1)/1000000, Memory+Mem2-Mem1 }
      end,
      { 0.0, 0 },
      lists:seq( 1, ?REPEAT )
    ),

  ?FORMAT( "  Elapsed time: ~f~n", [Time    /  ?REPEAT] ),
  ?FORMAT( "  Memory delta: ~b~n", [Memory div ?REPEAT] ).

%--------------------------------------------------------------