%--------------------------------------------------------------------
%
% Copyright (c) 2015 Mike French
%
% This software is released under the MIT license
% http://www.opensource.org/licenses/mit-license.php
%
%--------------------------------------------------------------------

-module( yazl_spec_tests ).

% ---------------------------------------------------
% Libraries

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

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

% ---------------------------------------------------
