-module(paypal_request).

-behaviour(gen_server).

-include("include/paypal.hrl").
%% API functions
-export([
         start_link/1,
         call/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          client_id = <<>> :: binary(),
          secret = <<>> :: binary(),
          token = <<>> :: binary(),
          env = sandbox :: sandbox | live,
          config = [] :: [proplists:property()]
               }).
-define(LIVE_URL, <<"https://api.paypal.com">>).
-define(SANDBOX_URL, <<"https://api.sandbox.paypal.com">>).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) -> % {{{1
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).


-spec call(Method, Function, Args) -> Response when % {{{1
      Method :: atom(),
      Function :: iodata(),
      Args :: map() | [proplists:property()],
      Response :: map() | [proplists:property()].
call(Method, Function, Args) -> 
    maybe_auth({Method, Function, Args},
      gen_server:call(?MODULE, {Method, Function, Args}, infinity)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Config]) -> % {{{1
    AppID = proplists:get_value(client_id, Config, <<>>),
    Secret = proplists:get_value(secret, Config, <<>>),
    lager:info("Starting vk handler for ~p", [ AppID ]),
    {ok, #state{client_id=AppID, config=Config, secret=Secret}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({auth}, _From, #state{client_id=AppID, secret=Secret}=State) -> % {{{1
    URL = hackney_url:make_url(get_url_for_env(State), [<<"v1">>, <<"oauth2">>, <<"token">>], []),
    Token = base64:encode(<<AppID/bytes, ":", Secret/bytes>>),
    lager:debug("AppID: ~pToken: ~p", [AppID, Token]),
    Headers = maps:to_list(
                #{<<"Accept">> => "application/json", 
                <<"Authorization">> => <<"Basic ", Token/bytes>>}),

    Responce = hackney:request(post, URL, Headers, {form, [{"grant_type", "client_credentials"}]}, [{follow_rediret, true}]),
     case decode_responce(Responce) of
         #{<<"access_token">> := AccessToken} = Map ->
             {reply, Map, State#state{token=AccessToken}};
         Map ->
             {reply, Map, State}
     end;
handle_call({Method, Function, Args}, _From, #state{token=Token}=State) -> % {{{1
    Headers = maps:to_list(
                #{<<"Content-Type">> => "application/json", 
                <<"Authorization">> => <<"Bearer ", Token/bytes>>}),

    {URL, Body} = request_data(Method, Function, Args, State),
    Responce = hackney:request(Method, URL, Headers, Body, [{follow_rediret, true}]),
    Map = decode_responce(Responce),
    {reply, Map, State};
handle_call(_Request, _From, State) -> % {{{1
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> % {{{1
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) -> % {{{1
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> % {{{1
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> % {{{1
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_responce({ok, 200, _, Ref}) ->  % {{{2
    Body = hackney:body(Ref),
    decode_body(Body);
decode_responce({ok, 401, _, Ref}) ->  % {{{2
    lager:warning("Authorization required: ~p", [hackney:body(Ref)]),
    unauthorized;
decode_responce({ok, Code, _, Ref}) ->  % {{{2
    Body = hackney:body(Ref),
    lager:warning("Request returned wrong code: ~p ~p", [Code, Body]),
    decode_body(Body);
decode_responce({error, Reason}) ->  % {{{2
    lager:warning("Request error: ~p", [Reason]),
    #{}.

decode_body({ok, Body}) ->  % {{{2
    try
        jsx:decode(Body, [return_maps])
    catch 
        error:badarg -> 
            lager:warning("JSON decode: ~p", [Body]),
            #{}
    end;
decode_body({error, Reason}) ->  % {{{2
    lager:warning("Body decode error: ~p", [Reason]),
    #{}.
parse_response(#{<<"response">> := [N | Data]}) when is_integer(N) ->  % {{{1
    {ok, Data};
parse_response(#{<<"response">> := Data}) ->  % {{{1
    {ok, Data};
parse_response(#{<<"error">> := Err}) ->  % {{{1
    lager:warning("VK error: ~p", [Err]),
    {error, Err}.

get_url_for_env(#state{env=sandbox}) -> % {{{1
    ?SANDBOX_URL;
get_url_for_env(#state{env=live}) -> % {{{1
    ?LIVE_URL.

maybe_auth(Request, unauthorized) -> % {{{1
      lager:debug("Auth: ~p", [gen_server:call(?MODULE, {auth}, infinity)]),
      gen_server:call(?MODULE, Request, infinity);
maybe_auth(_Request, Response) -> % {{{1
    Response.

request_data(get, Function, Args, State) -> % {{{1
    {hackney_url:make_url(get_url_for_env(State), [<<"v1">>, Function], Args), []};
request_data(_, Function, Args, State) -> % {{{1
    {hackney_url:make_url(get_url_for_env(State), [<<"v1">>, Function], []), Args}.

