%% Author: sls
%% Created: Jun 14, 2011
%% Description:
%% Parse JSON strings with the dot syntax
%% TODO: Generate readable error messages when selector is invalid
%% TODO: Optimize runtime 
-module(jdec).



%%
%% Macros
%%

-define(OP_STRUCT  , $.).
-define(OP_ARRAY   , $#).
-define(OP_MODIFIER, $$).



%%
%% Include files
%%

%-include("sls_helper.hrl").
-include("log_helper.hrl").
-include_lib("eunit/include/eunit.hrl").



%%
%% Exported Functions
%%
-export([
		 j1/0,
		 get/2,
		 get_s/2,
		 getft/2,
		 getft_s/2,
		 getio/2
		]).


%%
%% API Functions
%%


getio(JSON, SelOld) ->
	ResultNew = get(JSON, SelOld),
	io:format("~n", []),
	[begin io:format("Match:~p~n", [X]) end || X <- ResultNew],
	io:format("~n", []),
	ok.


get(JSON, SelOld) ->
	Object = try mochijson:decode(JSON)
			 catch 
				 ExA:ExB -> 
					 ?PF(e, ?L"mochijson throws '~p:~p' when decoding '~p'", [ExA, ExB, JSON]),
					 could_not_be_decoded_by_mochijson
			 end,
	getft(Object, SelOld).

get_s(JSON, SelOld) -> 
	case get(JSON, SelOld) of
		[A    ] -> A;
		[A | _] -> ?PF(e, ?L"get_s only works with a single selector"),
				   to_many_selectors_for_get_s
	end.

% get from term
getft(Object, SelOld) ->
	[Head | Tail] = g(Object, SelOld, []),
	
	% TODO: Do this before and remove this parenthesis pair from the Selector
	%       This would save some runtime.        
	case lists:last(SelOld) of 
		$) -> lists:reverse(Tail);
		_  -> lists:reverse([Head | Tail])
	end.


getft_s(Object, SelOld) -> 
	case getft(Object, SelOld) of
		[A    ] -> A;
		[A | _] -> ?PF(e, ?L"getft_s only works with a single selector"),
				   to_many_selectors_for_getft_s
	end.
	
%%
%% Local Functions
%%

g(ObjOld, [], AccOld) -> 
	[ObjOld | AccOld];

g(ObjOld, [?OP_MODIFIER, Modifier], AccOld) -> 
	ObjNew = case {Modifier, ObjOld} of 
				 {$l, {array,  Array}} -> Array;
				 {$l, {struct, Array}} -> Array;
				 _                     -> ObjOld
			 end,
	[ObjNew | AccOld];


g(ObjOld, [$( | SelTail], AccOld) -> 
	{Arg, SelNew} = parse_parenthesis(SelTail),
	AccNew = lists:append([
						   g(ObjOld, Arg, []),
						   AccOld 
						  ]),
	g(ObjOld, SelNew, AccNew);



g(ObjOld, [?OP_ARRAY | SelTail], AccOld) -> 
	{Arg, SelNew} = parse_array(SelTail),
	
	{array, Array} = ObjOld,
	
	N = list_to_integer(Arg),
	ObjNew = lists:nth(N, Array),
	g(ObjNew, SelNew, AccOld);


g(ObjOld, [?OP_STRUCT | SelTail], AccOld) -> 
	{Arg, SelNew} = parse_struct(SelTail),
	
	{struct, Proplist} = ObjOld,
	
	ObjNew = proplists:get_value(Arg, Proplist),
	g(ObjNew, SelNew, AccOld);



g(ObjOld, SelOld, AccOld) -> 
	['end'].


% returns {Argument, SelectorTail}
parse_parenthesis(Sel) -> parse_parenthesis([], Sel, 1).

parse_parenthesis(A, [$) | STail], 1    ) -> {lists:reverse(A), STail};
parse_parenthesis(A, [$) | STail], Level) -> parse_parenthesis([$) | A], STail, Level-1);
parse_parenthesis(A, [$( | STail], Level) -> parse_parenthesis([$( | A], STail, Level+1);
parse_parenthesis(A, [H  | STail], Level) -> parse_parenthesis([H  | A], STail, Level  ).



% returns {Argument, SelectorTail}
parse_struct(Sel) -> parse_struct([], Sel).

parse_struct(A, [                    ]) -> {lists:reverse(A), [                    ]};
parse_struct(A, [?OP_STRUCT   | STail]) -> {lists:reverse(A), [?OP_STRUCT   | STail]};
parse_struct(A, [?OP_ARRAY    | STail]) -> {lists:reverse(A), [?OP_ARRAY    | STail]};
parse_struct(A, [?OP_MODIFIER | STail]) -> {lists:reverse(A), [?OP_MODIFIER | STail]};
parse_struct(A, [$(           | STail]) -> {lists:reverse(A), [$(           | STail]};
parse_struct(A, [H            | STail]) -> parse_struct( [H | A], STail).


% returns {Argument, SelectorTail}
parse_array(Sel) -> parse_array([], Sel).

parse_array( A, [                    ]) -> {lists:reverse(A), [                    ]};
parse_array( A, [?OP_STRUCT   | STail]) -> {lists:reverse(A), [?OP_STRUCT   | STail]};
parse_array( A, [?OP_ARRAY    | STail]) -> {lists:reverse(A), [?OP_ARRAY    | STail]};
parse_array( A, [?OP_MODIFIER | STail]) -> {lists:reverse(A), [?OP_MODIFIER | STail]};
parse_array( A, [$(           | STail]) -> {lists:reverse(A), [$(           | STail]};
parse_array( A, [H            | STail]) -> parse_struct( [H | A], STail).






%%
%% Test Functions
%%


j1() -> 
     "{
       \"meta_schema_name\": \"real\",
       \"meta_schema_version\": 1,
       \"comments\":[
                      \"Bernhard\",
                      \"Tina\"
                    ],
       \"identifier\": {
         \"asset\": 45,
         \"aspect\": 12
       },
       \"payload\": {
         \"payload_schema_name\": \"real\",
         \"payload_schema_version\": 1,
         \"id\": 123,
         \"time\": \"2011-05-14T14:45:12.435691\",
         \"data\": {
           \"real\": 23.56456
         }
       }
     }".


parse_1_test() ->
	
	% struct
	S1 = "Heinz.klaus#4",
	?assert(parse_struct(S1) =:= {"Heinz", ".klaus#4"}),
	
	% parenthesis
	S2 = ".Heinz(#klara))",
	?assert(parse_parenthesis(S2) =:= {".Heinz(#klara)", ""}),
	
	ok.


get_1_test() ->
	J1 = "{\"a\":\"A\"}",
	?assert(get(J1, ".a") =:= ["A"]),
	
	J2 = "[11,12,13]",
	?assert(get(J2, "#2") =:= [12]),
	
	J3 = "{\"a\":[\"Kalle\",true, \"Paul\"]}",
	?assert(get(J3, ".a#3") =:= ["Paul"]),
	
	ok.


get_2_test() ->
	?assert(get(j1(), ".identifier.asset")          =:= [45]     ),
	?assert(get(j1(), ".identifier(.asset).aspect") =:= [45, 12] ),
	ok.


get_s_2_test() ->
	?assert(get_s(j1(), ".comments#2") =:= "Tina"                        ),
	?assert(get_s(j1(), ".comments"  ) =:= {array, ["Bernhard", "Tina"]} ),
	?assert(get_s(j1(), ".comments$l") =:= ["Bernhard", "Tina"]          ),
	ok.

