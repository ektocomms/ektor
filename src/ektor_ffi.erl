-module(ektor_ffi).
-export([
    new_handler/0, 'receive'/2, insert_handler/3, receive_forever_with_handler/2
]).


new_handler() ->
    {handler, #{}}.

insert_handler({handler, HandlerFns}, {inbox, Ref}, Fn) ->
    {handler, HandlerFns#{Ref => Fn}}.

'receive'({inbox, Ref}, Timeout) ->
    receive
        {Ref, Message} ->
            {ok, Message}
    after Timeout ->
        {error, nil}
    end.

receive_forever_with_handler(State, Handler) ->
    {ok, NewState} = receive_with_handler(State, Handler, infinity),
    NewState.

receive_with_handler(State, {handler, HandlerFns}, Timeout) ->
    receive
        % TODO: Review this adaptation
        % Monitored process down messages.
        % This is special cased so we can selectively receive based on the
        % reference as well as the record tag.
        % {'DOWN', Ref, process, Pid, Reason} when is_map_key(Ref, HandlerFns) ->
        %     Fn = maps:get(Ref, HandlerFns),
        %     {ok, Fn({process_down, Pid, Reason})};
        Msg when is_map_key(element(1, Msg), HandlerFns) ->
            Fn = maps:get(element(1, Msg), HandlerFns),
            {ok, Fn(element(2, Msg), State)}
        % Msg when AnythingHandler =/= undefined ->
        %     {ok, AnythingHandler(Msg)}
    after Timeout ->
        {error, nil}
    end.
