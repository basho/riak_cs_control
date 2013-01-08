%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Resource to serve static assets.

-module(riak_cs_control_wm_asset).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         to_resource/2,
         routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {resource=undefined, filename=undefined}).

%% @doc Initialize the resource.
init([]) ->
    {ok, #context{resource=undefined, filename=undefined}}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{[""], ?MODULE, []}, {['*'], ?MODULE, []}].

%% @doc Handle serving of the single page application.
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

%% @doc Given a series of request tokens, normalize to priv dir file.
-spec normalize_filepath(list()) -> list().
normalize_filepath(Filepath) ->
    {ok, App} = application:get_application(?MODULE),
    filename:join([code:priv_dir(App), 'www'] ++ Filepath).

%% @doc Return a context which determines if we serve up the application
%% template or a file resource.
identify_resource(ReqData, #context{resource=undefined}=Context) ->
    case wrq:disp_path(ReqData) of
        "" ->
            {true, Context#context{resource=template,
                                   filename=undefined}};
        _ ->
            Tokens = wrq:path_tokens(ReqData),
            Filename = normalize_filepath(Tokens),
            {true, Context#context{resource=filename,
                                   filename=Filename}}
    end;
identify_resource(_ReqData, Context) ->
    {true, Context}.

%% @doc If the file exists, allow it through, otherwise assume true if
%% they are asking for the application template.
resource_exists(ReqData, Context) ->
    case identify_resource(ReqData, Context) of
        {true, NewContext=#context{resource=template}} ->
            {true, ReqData, NewContext};
        {true, NewContext=#context{resource=filename,
                                   filename=Filename}} ->
            case filelib:is_regular(Filename) of
                true ->
                    {true, ReqData, NewContext};
                _ ->
                    {false, ReqData, NewContext}
            end
    end.

%% @doc Return the proper content type of the file, or default to
%% text/html.
content_types_provided(ReqData, Context) ->
    case identify_resource(ReqData, Context) of
        {true, NewContext=#context{resource=filename,
                                   filename=Filename}} ->
            MimeType = webmachine_util:guess_mime(Filename),
            {[{MimeType, to_resource}], ReqData, NewContext};
        {true, NewContext} ->
            {[{"text/html", to_resource}], ReqData, NewContext}
    end.

%% @doc Return the application template, or the contents of a file
%% resource.
to_resource(ReqData, Context) ->
    case identify_resource(ReqData, Context) of
        {true, NewContext=#context{resource=template}} ->
            Token = riak_cs_control_security:csrf_token(ReqData, Context),
            {ok, Content} = application_dtl:render([{csrf_token, Token}]),
            {Content,
             wrq:set_resp_header("Set-Cookie",
                                 "csrf_token="++Token++"; httponly", ReqData),
             NewContext};
        {true, NewContext=#context{resource=filename,
                                   filename=Filename}} ->
            {ok, Source} = file:read_file(Filename),
            {Source, ReqData, NewContext}
    end.
