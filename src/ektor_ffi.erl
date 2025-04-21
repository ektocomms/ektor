-module(ektor_ffi).

-export([new_handler_map/0, 'receive'/2, insert_handler/3, insert_anything_handler/2,
         receive_forever_with_handlers/2, merge_handler_maps/2]).

new_handler_map() ->
    {handler_map, #{}}.

merge_handler_maps({handler_map, HandlersA}, {handler_map, HandlersB}) ->
    {handler_map, maps:merge(HandlersA, HandlersB)}.

insert_anything_handler({handler_map, HandlerFns}, Fn) ->
    {handler_map, HandlerFns#{anything => Fn}}.

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
    {ok, Next} = receive_with_handlers(State, HandlerMap, infinity),
    Next.

receive_with_handlers(State, {handler_map, HandlerMap}, Timeout) ->
    AnythingHandler = maps:get(anything, HandlerMap, undefined),
    receive
        Msg when is_map_key(element(1, Msg), HandlerMap) ->
            Fn = maps:get(element(1, Msg), HandlerMap),
                {ok, Fn(element(2, Msg), State)};
        Msg when AnythingHandler =/= undefined ->
            {ok, AnythingHandler(Msg, State)}
    after Timeout ->
        {error, nil}
    end.
