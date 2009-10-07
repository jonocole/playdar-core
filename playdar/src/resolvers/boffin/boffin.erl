-module(boffin).

-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/3, weight/1, targettime/1, name/1]).
-export([tag/2, tag/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {tdb}).

%%

start_link()        -> gen_server:start_link(?MODULE, [], []).
tag()               -> tag(resolver:resolver_pid(?MODULE), library_dets:get_fdb()).
tag(Pid, Fdb)            -> gen_server:cast(Pid, {tag, Fdb}).
resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(_Pid)            -> 50.
targettime(_Pid)        -> 20.
name(_Pid)              -> "Boffin Tag Plugin".

%%

init([]) ->
    {ok, Tdb} = dets:open_file("tags.dets",[{type, duplicate_bag}]),
    ?LOG(info, "Boffin tag index contains ~w files",
               [proplists:get_value(size, dets:info(Tdb), -1)]),
    resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
    {ok, #state{tdb=Tdb}}.

handle_cast({resolve, Q, Qpid}, State) ->
    case Q of
        {struct, Mq} -> % Mq is a proplist
            ?LOG(debug, "Boffin resolving: ~p", [Mq]),
            Report = fun({Props, Score}) ->
                Rep =   {struct, [
                                {<<"artist">>, proplists:get_value(artist, Props)},
                                {<<"track">>,  proplists:get_value(track, Props)},
                                {<<"album">>,  proplists:get_value(album, Props)},
                                {<<"mimetype">>, proplists:get_value(mimetype, Props)},
                                {<<"score">>, Score},
                                {<<"url">>, proplists:get_value(url, Props)},
                                {<<"duration">>, proplists:get_value(duration, Props)},
                                {<<"bitrate">>, proplists:get_value(bitrate, Props)},
                                {<<"size">>, proplists:get_value(size, Props)},
                                {<<"source">>, <<"source_here">>}
                            ]},
                qry:add_result(Qpid, Rep)                
            end,
            Now = now(),
            %Do work
            Time = timer:now_diff(now(), Now),
            ?LOG(debug, "boffin search took: ~wms",[Time/1000]),
            qry:add_result(Qpid, {struct, [{<<"artist">>, <<"blah">>}]});
        _ -> noop %Unhandled query type
    end,
    {noreply, State};

handle_cast({tag, Fdb}, State ) ->
    AddFun = fun({File, Info}) ->
       Art = proplists:get_value( artist, Info ),
       Alb = proplists:get_value( album, Info ),
       Trk = proplists:get_value( track, Info ),
       {continue, {File,Art,Alb,Trk}}
    end,

    L = dets:traverse( Fdb, AddFun ),
    Query = prepare_tag_query( L ),
    {ok,{_ResponseCode, _Headers, Body}} = 
        http:request( post, {"http://musiclookup.last.fm/trackresolve", [], "text/plain", Query}, [], []),

    parse_tag_response( Body, L , State),
    {noreply, State}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    mnesia:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal

prepare_tag_query( L ) -> prepare_tag_query( L, <<>>, 1 ).

prepare_tag_query( [], Query, _ ) -> Query;

prepare_tag_query( L, Query, Count ) ->
    [{_File, Art, Alb, Trk} | R] = L,
    CountString = list_to_binary(integer_to_list(Count)),
    Q = <<Query/binary, CountString/binary, $\t, Art/binary, $\t, Alb/binary, $\t, Trk/binary, $\n>>,
    prepare_tag_query( R, Q, Count + 1 ).

parse_tag_response( Response, Tracks, State) ->
    Lines = string:tokens( Response, [$\n] ),
    lists:foreach( fun(L) -> parse_tag_response_line(L, Tracks, State) end, Lines),
    dets:sync( State#state.tdb ).

parse_tag_response_line( Line, Tracks, State ) ->
    [Id | Tokens] = string:tokens( Line, [$\t] ),
    {File, _Art, _Alb, _Trk} = lists:nth( list_to_integer( Id ), Tracks ),
    add_tags( File, Tokens, State ).

add_tags( _File, [], _State ) -> ok;

add_tags( File, Tokens, State ) ->
    [Tag, WeightString | Rest ] = Tokens,

    Weight = try list_to_float( WeightString ) 
    catch _:_ ->
            list_to_integer( WeightString )
    end,

    dets:insert( State#state.tdb, {Tag, {File, Weight }}),
    add_tags( File, Rest, State ).

tag_cloud( State ) ->
    Tags = dets:traverse( State#state.tdb, fun({Key, {_File, Weight}) -> 
        {continue, {Key, Weight}}
    end ),
    io:format( "Tags:~n~p", [Tags] ).
