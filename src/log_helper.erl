%% Author: sejnub
%% Created: May 26, 2011
-module(log_helper).

-export([pf/2, pf/3, pf/4]).


pf(        Format, 	Module) when is_list(Format)                     -> pf(i  , Format, [],		Module );
pf(        Data, 	Module)                                          -> pf(i  , []    , Data,	Module ).

pf(IWE   , Format, 	Module) when is_atom(IWE   ) and is_list(Format) -> pf(IWE, Format, [],		Module );
pf(Format, Data,	Module) when is_list(Format) and is_list(Data  ) -> pf(i  , Format, Data,	Module ).

pf(IWE, Format, Data, Module) -> log_it(IWE, [{module, Module}] ++ Format, Data). % IWE = i (for info), w (for warning), e (for error)




log_it(IWE, Format, Data )                                                                             -> log_it([]                                          , IWE, Format, Data ).

log_it(_Accu                              , IWE, [{logging_function_and_line, TagData}|Format], Data ) -> log_it([{logging_function_and_line, TagData}      ], IWE, Format, Data );
log_it(Accu                               , IWE, [{Tag, TagData}|Format]                      , Data ) -> log_it([{Tag, TagData}                      | Accu], IWE, Format, Data );
log_it(Accu                               , IWE, []                                           , []   ) -> log_it_2(IWE, lists:reverse(Accu)                                                               );
log_it([]                                 , IWE, [Head|Tail]                                  , Data ) -> log_it_2(IWE, io_lib:format(                                             [Head|Tail], Data)     );
log_it([{logging_function_and_line, MFAL}], IWE, [Head|Tail]                                  , Data ) -> log_it_2(IWE, io_lib:format(             MFAL                 ++ ": " ++ [Head|Tail], Data)     );
log_it([{module, Module}]                 , IWE, [Head|Tail]                                  , Data ) -> log_it_2(IWE, io_lib:format("Module " ++ atom_to_list(Module) ++ ": " ++ [Head|Tail], Data)     );
log_it(Accu                               , IWE, [Head|Tail]                                  , Data ) -> log_it_2(IWE, lists:reverse([{'Reason', lists:flatten(io_lib:format([Head|Tail], Data))}|Accu]) );
log_it(Accu                               , IWE, []                                           , Data ) -> log_it_2(IWE, lists:reverse([{'Data', Data}|Accu])                                              ).


log_it_2(i, Report) -> error_logger:info_report(   Report);
log_it_2(w, Report) -> error_logger:warning_report(Report);
log_it_2(e, Report) -> error_logger:error_report(  Report).


