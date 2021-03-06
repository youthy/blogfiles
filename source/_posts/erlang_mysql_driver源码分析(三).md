title: erlang_mysql_driver源码分析(三)
date: 2015-06-10 15:37:35
tags: erlang
categories: erlang
---
<!-- toc -->
#mysql_recv:start_link

```
init(Host, Port, User, Password, Database, LogFun, Encoding, PoolId, Parent) ->
    case mysql_recv:start_link(Host, Port, LogFun, self()) of
	{ok, RecvPid, Sock} ->
	    case mysql_init(Sock, RecvPid, User, Password, LogFun) of
		{ok, Version} ->
```

上次在mysql_conn:init里面调用了mysql_recv:start_link,我们知道返回一个RecvPid接收进程的Pid和Sock套接字。mysql_recv:start_link过程如下
<!--more-->

```
start_link(Host, Port, LogFun, Parent) when is_list(Host), is_integer(Port) ->
    RecvPid =
	spawn_link(fun () ->
			   init(Host, Port, LogFun, Parent)
		   end),
    %% wait for the socket from the spawned pid
    receive
	{mysql_recv, RecvPid, init, {error, E}} ->
	    {error, E};
	{mysql_recv, RecvPid, init, {ok, Socket}} ->
	    {ok, RecvPid, Socket}
    after ?CONNECT_TIMEOUT ->
	    catch exit(RecvPid, kill),
	    {error, "timeout"}
    end.
```

与mysql_conn大体相同，同样spawn一个进程，将mysql_conn的Pid传入当做Parent父进程，然后mysql_conn等待receive消息，如果spawn的mysql_recv进程返回{mysql_recv, RecvPid, init, {ok, Socket}},就代表创建成功，然后将RecvPid, Socket这些有用的变量返回，以便于mysql_init进行,
如果超过?CONNECT_TIMEOUT的时间，则会调用exit(RecvPid, kill)关闭mysql_recv进程，并返回{error, "timeout"}.

```
init(Host, Port, LogFun, Parent) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
	{ok, Sock} ->
	    Parent ! {mysql_recv, self(), init, {ok, Sock}},
	    State = #state{socket  = Sock,
			   parent  = Parent,
			   log_fun = LogFun,
			   data    = <<>>
			  },
	    loop(State);
	E ->
	    LogFun(?MODULE, ?LINE, error,
		   fun() ->
			   {"mysql_recv: Failed connecting to ~p:~p : ~p",
			    [Host, Port, E]}
		   end),
	    Msg = lists:flatten(io_lib:format("connect failed : ~p", [E])),
	    Parent ! {mysql_recv, self(), init, {error, Msg}}
    end.
```

init里面通过gen_tcp:connect连接了端口，返回套接字，将这个套接字发送给父进程也就是在上面receive的地方，之后进入loop。如果连接失败，打印错误，并通知父进程。
> state{socket,   %套接字
        parent,   %父进程pid
        log_fun,  %打印日志函数
        data,     %端口发来的数据
}

```
loop(State) ->
    Sock = State#state.socket,
    receive
	{tcp, Sock, InData} ->
	    NewData = list_to_binary([State#state.data, InData]),
	    %% send data to parent if we have enough data
	    Rest = sendpacket(State#state.parent, NewData),
	    loop(State#state{data = Rest});
	{tcp_error, Sock, Reason} ->
	    LogFun = State#state.log_fun,
	    LogFun(?MODULE, ?LINE, error,
		   fun() ->
			   {"mysql_recv: Socket ~p closed : ~p",
			    [Sock, Reason]}
		   end),
	    State#state.parent ! {mysql_recv, self(), closed, {error, Reason}},
	    error;
	{tcp_closed, Sock} ->
	    LogFun = State#state.log_fun,
	    LogFun(?MODULE, ?LINE, debug,
		   fun() ->
			   {"mysql_recv: Socket ~p closed", [Sock]}
		   end),
	    State#state.parent ! {mysql_recv, self(), closed, normal},
	    error
    end.
```

gen_tcp:connect没有指定{active, false},这样的话，端口可以主动发给recv进程消息。实际上在在gen_tcp:connect之后我们得到Socket的同时，mysql_recv进程的消息队列里已经有Socket的发来的消息了。我们在shell里面可以试一下
```
1> gen_tcp:connect("localhost", 3306, [binary, {packet, 0}]).
{ok,#Port<0.595>}
2> rp(process_info(self())).
[{current_function,{erl_eval,do_apply,6}},
 {initial_call,{erlang,apply,2}},
 {status,running},
 {message_queue_len,2},
 {messages,[{tcp,#Port<0.595>,
                 <<91,0,0,0,10,53,46,53,46,51,55,45,48,117,98,117,110,116,
                   117,48,46,49,52,46,48,52,46,49,0,187,1,0,0,57,98,97,67,
                   104,98,52,123,0,255,247,8,2,0,15,128,21,0,0,0,0,0,0,0,
                   0,0,0,50,36,92,38,67,96,59,67,125,112,59,115,0,109,121,
                   115,113,108,95,110,97,116,105,118,101,95,112,97,115,
                   115,119,111,114,100,0>>},
            {tcp_closed,#Port<0.595>}]},
 {links,[<0.26.0>]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.25.0>},
 {total_heap_size,3571},
 {heap_size,987},
 {stack_size,37},
 {reductions,4070},
 {garbage_collection,[{min_bin_vheap_size,46368},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,5}]},
 {suspending,[]}]
ok
```

我们所做的操作以mysql_recv:init里面是一样的。打开了3306端口。这是shell的messagebox里面已经有了两天Socket发来的消息。loop函数将这段消息交给了sendpacket处理

```
sendpacket(Parent, Data) ->
    case Data of
	<<Length:24/little, Num:8, D/binary>> ->
	    if
		Length =< size(D) ->
		    {Packet, Rest} = split_binary(D, Length),
		    Parent ! {mysql_recv, self(), data, Packet, Num},
		    sendpacket(Parent, Rest);
		true ->
		    Data
	    end;
	_ ->
	    Data
    end.
```

packet头3个字节表示数据长度，第四个字节是帧序号，之后所有的是数据。那么这段数据头三个<<91,0,0>>代表数据长度是91字节（小端存储），之后的0代表这个帧序号是0，<<10....>>就是真正的数据。sendpacket将这段数据发给了parent。在如下的地方匹配
```
init(Host, Port, User, Password, Database, LogFun, Encoding, PoolId, Parent) ->
    case mysql_recv:start_link(Host, Port, LogFun, self()) of
	{ok, RecvPid, Sock} ->
	    case mysql_init(Sock, RecvPid, User, Password, LogFun) of
		{ok, Version} ->
------
mysql_init(Sock, RecvPid, User, Password, LogFun) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, Packet, InitSeqNum} ->
		    {Version, Salt1, Salt2, Caps} = greeting(Packet, LogFun),
-----
do_recv(LogFun, RecvPid, SeqNum)  when is_function(LogFun);
				       LogFun == undefined,
				       SeqNum == undefined ->
    receive
        {mysql_recv, RecvPid, data, Packet, Num} ->
	    {ok, Packet, Num};
	{mysql_recv, RecvPid, closed, _E} ->
	    {error, "mysql_recv: socket was closed"}
    end;
```

在mysql_conn的do_recv完成对消息的匹配。
上面的数据其实就是
```
=INFO REPORT==== 10-Jun-2015::11:59:21 ===
I(<0.116.0>:mysql_conn:620) : greeting packet <<10,53,46,53,46,51,55,45,48,117,
                                                98,117,110,116,117,48,46,49,52,
                                                46,48,52,46,49,0,173,1,0,0,100,
                                                44,77,124,75,63,60,78,0,255,
                                                247,8,2,0,15,128,21,0,0,0,0,0,
                                                0,0,0,0,0,84,60,110,54,95,93,
                                                55,57,88,46,122,66,0,109,121,
                                                115,113,108,95,110,97,116,105,
                                                118,101,95,112,97,115,115,119,
                                                111,114,100,0>> version "5.5.37-0ubuntu0.14.04.1" (protocol 10) salt "d,M|K?<N" caps 63487 serverchar <<8,2,0,
                                                                                                                                                        15,128,
                                                                                                                                                        21,0,0,
                                                                                                                                                        0,0,0,0,
                                                                                                                                                        0,0,0,0>>salt2 "T<n6_]79X.zB"
```

greeting函数用来转化为版本号，salt等参数的。
```
greeting(Packet, LogFun) ->
    <<Protocol:8, Rest/binary>> = Packet,
    {Version, Rest2} = asciz(Rest),
    <<_TreadID:32/little, Rest3/binary>> = Rest2,
    {Salt, Rest4} = asciz(Rest3),
    <<Caps:16/little, Rest5/binary>> = Rest4,
    <<ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
    {Salt2, _Rest7} = asciz(Rest6),
    ?Log2(LogFun, debug,
	  "greeting packet ~p version ~p (protocol ~p) salt ~p caps ~p serverchar ~p"
	  "salt2 ~p",
	  [Packet, Version, Protocol, Salt, Caps, ServerChar, Salt2]),
    {normalize_version(Version, LogFun), Salt, Salt2, Caps}.
------------------------------------------
asciz(Data) when is_binary(Data) ->
    mysql:asciz_binary(Data, []).
-----------------------------------------
asciz_binary(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
    asciz_binary(Rest, [C | Acc]).
```

Packet第一个字节是protocol,之后asciz函数将剩下的二进制以0位分界点分开。那么
Packet被分成了
> Protocol : <<10>>
Version:<<53,46,53,46,51,55,45,48,117,98,117,110,116,117,48,46,49,52,46,48,52,46,49>>,
Salt:<<100,44,77,124,75,63,60,78>>
Caps:<<255,247>>
ServerChar:<<15,128,21,0,0,0,0,0,0,0,0,0,0,84,60,110>>(ServerChar是16个字节不是16个bit)
Salt2:<<97,116,105,118,101,95,112,97,115,115,119,111,114,100>>

在shell中检测下
```
4> <<53,46,53,46,51,55,45,48,117,98,117,110,116,117,48,46,49,52,46,48,52,46,49>>.
<<"5.5.37-0ubuntu0.14.04.1">>
5> <<100,44,77,124,75,63,60,78>>.
<<"d,M|K?<N">>
6> <<Caps:16/little>> = <<255,247>>.
<<"ÿ÷">>
7> Caps.
63487
```

和上面打印的log是一样的。
