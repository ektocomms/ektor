-module(ektor_ffi).

-export([new_topic_router/0, 'receive'/2, insert_handler/3, insert_anything_handler/2,
         receive_forever_with_router/2, merge_topic_routers/2]).

new_topic_router() ->
    {topic_router, #{}}.

merge_topic_routers({topic_router, RouterA}, {topic_router, RouterB}) ->
    {topic_router, maps:merge(RouterA, RouterB)}.

insert_anything_handler({topic_router, HandlerFns}, Fn) ->
    {topic_router, HandlerFns#{anything => Fn}}.

insert_handler({topic_router, HandlerFns}, {topic, Ref}, Fn) ->
    {topic_router, HandlerFns#{Ref => Fn}}.

'receive'({topic, Ref}, Timeout) ->
    receive
        {Ref, Message} ->
            {ok, Message}
    after Timeout ->
        {error, nil}
    end.

receive_forever_with_router(State, TopicRouter) ->
    {ok, Next} = receive_with_router(State, TopicRouter, infinity),
    Next.

receive_with_router(State, {topic_router, TopicRouter}, Timeout) ->
    AnythingHandler = maps:get(anything, TopicRouter, undefined),
    receive
        Msg when is_map_key(element(1, Msg), TopicRouter) ->
            Fn = maps:get(element(1, Msg), TopicRouter),
                {ok, Fn(element(2, Msg), State)};
        Msg when AnythingHandler =/= undefined ->
            {ok, AnythingHandler(Msg, State)}
    after Timeout ->
        {error, nil}
    end.
