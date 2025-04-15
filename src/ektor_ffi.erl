-module(ektor_ffi).
-export([
    new_handler_map/0, 'receive'/2, insert_handler/3, receive_forever_with_handlers/2
]).


new_handler_map() ->
    {handler_map, #{}}.

insert_handler({handler_map, HandlerFns}, {inbox, Ref}, Fn) ->
    {handler_map, HandlerFns#{Ref => Fn}}.

'receive'({inbox, Ref}, Timeout) ->
    receive
        {Ref, Message} ->
            {ok, Message}
    after Timeout ->
        {error, nil}
    end.

receive_forever_with_handlers(State, HandlerMap) ->
    {ok, NewState} = receive_with_handlers(State, HandlerMap, infinity),
    NewState.

receive_with_handlers(State, {handler_map, HandlerMap}, Timeout) ->
    receive
        % TODO: Review this adaptation
        % Monitored process down messages.
        % This is special cased so we can selectively receive based on the
        % reference as well as the record tag.
        % {'DOWN', Ref, process, Pid, Reason} when is_map_key(Ref, HandlerFns) ->
        %     Fn = maps:get(Ref, HandlerFns),
        %     {ok, Fn({process_down, Pid, Reason})};
        Msg when is_map_key(element(1, Msg), HandlerMap) ->
            Fn = maps:get(element(1, Msg), HandlerMap),
            {ok, Fn(element(2, Msg), State)}
        % Msg when AnythingHandler =/= undefined ->
        %     {ok, AnythingHandler(Msg)}
    after Timeout ->
        {error, nil}
    end.
