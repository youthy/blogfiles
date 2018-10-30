title: Erlang实现多人聊天室模型
date: 2014-06-26 18:31:04
tags: erlang
categories: erlang
---
##Erlang程序设计课后习题,模拟聊天室代码.

> useage:用终端打开多个节点其中一个用于启动聊天服务器,在erl环境中调用``chat_room:start()``启动,之后其他节点运行``telnet localhost 8000``登录聊天室服务器,在其中一个节点发送消息,其他节点能收到.

<!--more-->

```Erlang
-module(chat_room).
-behaviour(gen_server).
-export([start/0, stop/0, lists/0]).

%gen_server handle export function declaration
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).
%start the gen_server for saving the Socketlist
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:cast(?MODULE, stop).

%add the new Socket to Socket_list
add(Socket) -> 
    gen_server:cast(?MODULE, {add, Socket}).

%delete Socket in Socketlist
delete_port(Socket) -> 
    gen_server:cast(?MODULE, {delete, Socket}).

%get Socketlist
lists() -> gen_server:call(?MODULE, list).

%%----------------------------------------------------
%% handle functions
%%----------------------------------------------------

init([]) ->
    Listen = gen_listensocket(),
    io:format("beginning listen ~p~n", [Listen]),
    {ok, {Listen, []}}.

handle_cast({delete, Socket}, {Listen, Socket_list}) ->
    Socket_list1=lists:delete(Socket, Socket_list),
    {noreply, {Listen, Socket_list1}};
handle_cast({add, Socket}, {Listen,Socket_list}) ->
    Socket_list1 = [Socket | Socket_list],
    {noreply, {Listen, Socket_list1}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(list, _From, State) ->
    {_, Reply} = State,
    {reply, Reply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {Listen, Sockets}) ->
    io:format("listen~p~n" ,[Listen]),
    gen_tcp:close(Listen),
    [gen_tcp:close(S) || S <- Sockets],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------
%%  internal functions 
%%----------------------------------------------------

%get the listen socket,new a thread to wait clients to connect.
gen_listensocket() ->
    {ok, Listen} = gen_tcp:listen(8000, [binary, {active, false}, {reuseaddr, true}]),
    spawn(fun() -> gen_socket(Listen) end),
    Listen.

%do loop() to Socket and new a thread to wait clients to connect
gen_socket(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} -> 
            add(Socket),
            spawn(fun() -> gen_socket(Listen) end),
            loop(Socket);
        {error, closed} -> exit
    end.

%send information from Socket to other Socket in Socketlist
loop(Socket) -> 
    case gen_tcp:recv(Socket, 0) of 
        {ok, Bin} ->
            [gen_tcp:send(X, Bin) || X <- lists(), X /= Socket],
            loop(Socket);
        {error, closed} ->
            delete_port(Socket)
    end.```


 
