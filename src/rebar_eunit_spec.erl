-module(rebar_eunit_spec).

-behaviour(eunit_listener).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
%% where does this come from?
%%-include_lib("eunit/include/eunit_internal.hrl").

-export([start/0, start/1, test/2, test/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
	 terminate/2]).

-record(state, {use_color = true,
		indent = 0
	       }).

test(Tests) ->
    test(Tests, []).

test(Tests, Options) ->
    %% Listeners = [eunit_tty:start(Options) | listeners(Options)],
    Listeners = [?MODULE:start(Options)],
    Serial = eunit_serial:start(Listeners),
    case eunit_server:start_test(eunit_server, Serial, Tests, Options) of
	{ok, Reference} -> test_run(Reference, Listeners);
	{error, R} -> {error, R}
    end.


start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    St = #state{use_color = proplists:get_value(use_color, Options, true)},
    receive
	{start, _Reference} ->
            print_header(),
	    St
    end.

terminate({ok, Data}, St) ->
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    if Fail =:= 0, Skip =:= 0, Cancel =:= 0 ->
	    if Pass =:= 0 ->
		    io:fwrite("  There were no tests to run.\n");
	       true ->
                    print_bar(),
		    Summary = if Pass =:= 1 ->
                                      io_lib:fwrite("  Test passed.\n");
                                 true ->
                                      io_lib:fwrite("  All ~w tests passed.\n",
                                                    [Pass])
		    end,
                    io:fwrite(color(Summary, green))
	    end,
	    sync_end(ok);
       true ->
	    print_bar(),
            Summary =
                io_lib:fwrite("  Failed: ~w.  Skipped: ~w.  Passed: ~w.\n",
                              [Fail, Skip, Pass]),
	    io:fwrite(color(Summary, red)),
	    if Cancel =/= 0 ->
		    io:fwrite("~s\n",
                              [color("One or more tests were cancelled.",
                                     yellow)]);
	       true -> ok
	    end,
	    sync_end(error)
    end;
terminate({error, Reason}, _St) ->
    io:fwrite("Internal error: ~P.\n", [Reason, 25]),
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

print_header() ->
    io:fwrite("~s\n",
              [color("======================== EUnit ========================", blue)]).

print_bar() ->
    io:fwrite("=======================================================\n").


handle_begin(group, Data, St) ->
    ?debugFmt("handle_begin group ~w", [Data]),
    Desc = proplists:get_value(desc, Data),
    if Desc =/= "", Desc =/= undefined ->
	    I = St#state.indent,
	    print_group_start(I, Desc),
	    St#state{indent = I + 1};
       true ->
	    St
    end;
handle_begin(test, _Data, St) ->
    nothing,
    St.
    %% ?debugFmt("handle_begin test ~w", [Data]),
    %% io:format("Data: ~p~n~n", [Data]),
    %% if St#state.verbose -> print_test_begin(St#state.indent, Data);
    %%    true -> ok
    %% end,
    %% St.

handle_end(group, Data, St) ->
    ?debugFmt("handle_end group ~w", [Data]),
    Desc = proplists:get_value(desc, Data),
    if Desc =/= "", Desc =/= undefined ->
	    Time = proplists:get_value(time, Data),
	    I = St#state.indent,
	    print_group_end(I, Time),
	    St#state{indent = I - 1};
       true ->
	    St
    end;
handle_end(test, Data, St) ->
    ?debugFmt("handle_end test ~w", [Data]),
    print_test_begin(St#state.indent, Data),
    case proplists:get_value(status, Data) of
        ok ->
            ok;
        Status ->
            print_test_error(Status, Data)
    end,
    St.
    %% case proplists:get_value(status, Data) of
    %%     ok ->
    %%         if St#state.verbose ->
    %%                 print_test_begin(St#state.indent, Data)
    %%                 %% print_test_end(Data);
    %%            true -> ok
    %%         end,
    %%         St;
    %%     Status ->
    %%         if St#state.verbose -> ok;
    %%            true -> print_test_begin(St#state.indent, Data)
    %%         end,
    %%         print_test_error(Status, Data),
    %%         St
    %% end.

handle_cancel(group, Data, St) ->
    ?debugFmt("handle_cancel group ~w", [Data]),
    I = St#state.indent,
    case proplists:get_value(reason, Data) of
	undefined ->
	    %% "skipped" message is not interesting here
	    St#state{indent = I - 1};
	Reason ->
	    Desc = proplists:get_value(desc, Data),
	    if Desc =/= "", Desc =/= undefined ->
		    print_group_cancel(I, Reason);
	       true ->
		    print_group_start(I, Desc),
		    print_group_cancel(I, Reason)
	    end,
	    St#state{indent = I - 1}
    end;
handle_cancel(test, Data, St) ->
    ?debugFmt("handle_cancel test ~w", [Data]),
    print_test_begin(St#state.indent, Data),
    print_test_cancel(proplists:get_value(reason, Data)),
    St.


indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_N) ->
    ok.

print_group_start(I, Desc) ->
    indent(I),
    io:fwrite("~s\n", [Desc]).

print_group_end(I, Time) ->
    if Time > 0 ->
	    indent(I),
	    io:fwrite("[done in ~.3f s]\n", [Time/1000]);
       true ->
	    ok
    end.

-define(end_escape, "\033[0m").

color(S, Color) ->
    Escape = escape_for_color(Color),
    io_lib:fwrite("~s~s~s", [Escape, S, ?end_escape]).

escape_for_color(green) ->
    "\033[92m";
escape_for_color(red) ->
    "\033[91m";
escape_for_color(blue) ->
    "\033[94m";
escape_for_color(yellow) ->
    "\033[93m".

color_for_status(ok) ->
    green;
color_for_status(_) ->
    red.
    
print_test_begin(I, Data) ->
    Desc = proplists:get_value(desc, Data),
    Line = proplists:get_value(line, Data, 0),
    indent(I),
    L = if Line =:= 0 -> "";
	   true -> io_lib:fwrite("(L:~w)", [Line])
	end,
    if Desc =:= ""; Desc =:= undefined ->
            "";
       true ->
            Color = color_for_status(proplists:get_value(status, Data)),
            io:fwrite("~s ~s~n", [color(Desc, Color), L])
    end.

%% print_test_end(_Data) ->
%%     %% Time = proplists:get_value(time, Data, 0),
%%     %% T = if Time > 0 -> io_lib:fwrite("[~.3f s] ", [Time/1000]);
%%     %%        true -> ""
%%     %%     end,
%%     nothing.
%%     %% io:fwrite("\n").

print_test_error({error, Exception}, Data) ->
    Output = proplists:get_value(output, Data),
    io:fwrite("~s\n::~s",
	      [color("*failed*", red),
               eunit_lib:format_exception(Exception)]),
    case Output of
	<<>> ->
	    io:put_chars("\n\n");
	<<Text:800/binary, _:1/binary, _/binary>> ->
	    io:fwrite("  output:<<\"~s\">>...\n\n", [Text]);
	_ ->
	    io:fwrite("  output:<<\"~s\">>\n\n", [Output])
    end;
print_test_error({skipped, Reason}, _) ->
    io:fwrite("*did not run*\n::~s\n", [format_skipped(Reason)]).

format_skipped({module_not_found, M}) ->
    io_lib:format("missing module: ~w", [M]);
format_skipped({no_such_function, {M,F,A}}) ->
    io_lib:format("no such function: ~w:~w/~w", [M,F,A]).    

print_test_cancel(Reason) ->
    io:fwrite(format_cancel(Reason)).

print_group_cancel(_I, {blame, _}) ->
    ok;
print_group_cancel(I, Reason) ->
    indent(I),
    io:fwrite(format_cancel(Reason)).

format_cancel(undefined) ->
    "*skipped*\n";
format_cancel(timeout) ->
    "*timed out*\n";
format_cancel({startup, Reason}) ->
    io_lib:fwrite("*could not start test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({blame, _SubId}) ->
    "*cancelled because of subtask*\n";
format_cancel({exit, Reason}) ->
    io_lib:fwrite("*unexpected termination of test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({abort, Reason}) ->
    eunit_lib:format_error(Reason).



%% copied
listeners(Options) ->
    Ps = start_listeners(proplists:get_all_values(report, Options)),
    %% the event_log option is for debugging, to view the raw events
    case proplists:get_value(event_log, Options) of
	undefined ->
	    Ps;
	X ->
	    LogFile = if is_list(X) -> X;
			 true -> "eunit-events.log"
		      end,
	    [spawn_link(fun () -> event_logger(LogFile) end) | Ps]
    end.

start_listeners([P | Ps]) when is_pid(P) ; is_atom(P) ->
    [P | start_listeners(Ps)];
start_listeners([{Mod, Opts} | Ps]) when is_atom(Mod) ->
    [Mod:start(Opts) | start_listeners(Ps)];	    
start_listeners([]) ->
    [].

%% TODO: make this report file errors
event_logger(LogFile) ->
    case file:open(LogFile, [write]) of
	{ok, FD} ->
	    receive
		{start, Reference} ->
		    event_logger_loop(Reference, FD)
	    end;
	Error ->
	    exit(Error)
    end.

event_logger_loop(Reference, FD) ->
    receive
	{status, _Id, _Info}=Msg ->
	    io:fwrite(FD, "~p.\n", [Msg]),
	    event_logger_loop(Reference, FD);
	{stop, Reference, _ReplyTo} ->
	    %% no need to reply, just exit
	    file:close(FD),
	    exit(normal)
    end.


test_run(Reference, Listeners) ->
    receive
	{start, Reference} ->
	    cast(Listeners, {start, Reference})
    end,
    receive
	{done, Reference} ->
	    cast(Listeners, {stop, Reference, self()}),
            wait_until_listeners_have_terminated(Listeners),
	    receive
		{result, Reference, Result} ->
		    Result
	    end
    end.

cast([P | Ps], Msg) ->
    P ! Msg,
    cast(Ps, Msg);
cast([], _Msg) ->
    ok.

wait_until_listeners_have_terminated([P | Ps]) ->
    MRef = erlang:monitor(process, P),
    receive
        {'DOWN', MRef, process, P, _} ->
            wait_until_listeners_have_terminated(Ps)
    end;
wait_until_listeners_have_terminated([]) ->
    ok.
