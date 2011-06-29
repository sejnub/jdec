-define(PF(P1        ), log_helper:pf(P1        , ?MODULE)).
-define(PF(P1, P2    ), log_helper:pf(P1, P2    , ?MODULE)).
-define(PF(P1, P2, P3), log_helper:pf(P1, P2, P3, ?MODULE)).

% For a parse_transform solution for ?FUNCTION 
% see http://erlang.2086793.n4.nabble.com/Function-name-macro-the-counterpart-of-MODULE-td2099690.html
-define(FUNCTION, case process_info(self(),current_function) of 
					  {current_function, {M,F,A}} -> atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)
				  end	   
	   ).

% OK, I admit that this isn't nice. But practical if you want to prepend 
% the calling function to an error message you want to log.
-define(L, [{logging_function_and_line, ?FUNCTION ++ " " ++ integer_to_list(?LINE)}] ++  ).


