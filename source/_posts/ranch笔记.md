title: ranch笔记
date: 2015-09-28 11:58:51
tags: erlang
categories: erlang
thumbnail: "/img/ranch_3.png"

---

任何使用ranch的程序第一步需要启动ranch_app
## start ranch application
入口
`ranch_app:`
```
start(_, _) ->
	_ = consider_profiling(), %% 是否启动eprof
	ranch_sup:start_link().
```

<!-- more -->

`ranch_sup:` 
整个应用的最顶级sup， 它建立ranch_server ets,并启动ranch_server这个gen_server,将ranch_server这个ets归属于ranch_sup提高容错.防止数据丢失。
```
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ranch_server = ets:new(ranch_server, [
		ordered_set, public, named_table]),
	Procs = [
		{ranch_server, {ranch_server, start_link, []},
			permanent, 5000, worker, [ranch_server]}
	],
	{ok, {{one_for_one, 10, 10}, Procs}}.
```

`ranch_server:`
是一个gen_server,init中从ranch_server ets中恢复数据
```
init([]) ->
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} ||
		[Ref, Pid] <- ets:match(?TAB, {{conns_sup, '$1'}, '$2'})],
	{ok, #state{monitors=Monitors}}.
```
**ranch初始化完成**
![](/img/ranch_1.png)
----
至此ranch实际上没有做任何事情，它并没有监听任何端口，需要使用者在自己的app中显示的调用ranch:start_listener来启动acceptor_pool.

在作者的demo中是这样：
### tcp_echo_app:
```
start(_Type, _Args) ->
	{ok, _} = ranch:start_listener(tcp_echo, 1,
		ranch_tcp, [{port, 5555}], echo_protocol, []),
	tcp_echo_sup:start_link().
```

`ranch:start_listner`接受6个参数 
1. 名字: tcp_echo,
2. acceptor数量: 1
**此处不是connection的数量!! wiki说:**
> First of all, it should not be confused with the maximum number of connections. Acceptor processes are only used for accepting and have nothing else in common with connection processes. Therefore there is nothing to be gained from setting this number too high, in fact it can slow everything else down.

> Second, this number should be high enough to allow Ranch to accept connections concurrently. But the number of cores available doesn’t seem to be the only factor for choosing this number, as we can observe faster accepts if we have more acceptors than cores. It might be entirely dependent on the protocol, however.

总之就是太少太多都不好，他们的观测得到100较为合适

3. transport的方式: ranch_tcp
4. transport options: [{port, 5555}], 
`ranch_tcp:`
```
listen(Opts) ->
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	Opts3 = ranch:set_option_default(Opts2, nodelay, true),
	Opts4 = ranch:set_option_default(Opts3, send_timeout, 30000),
	Opts5 = ranch:set_option_default(Opts4, send_timeout_close, true),
	gen_tcp:listen(0, ranch:filter_options(Opts5, listen_options(),
		[binary, {active, false}, {packet, raw}, {reuseaddr, true}])).
```
可以看出ranch_tcp代码中默认使用binary, {active,false}, 等设置，所以无法通过options更改。可以在start_listener之后的通过Transport:setopts/2更改.
但是backlog, nodelay等可以通过Opts在start的时候就设置好.
**port**:未指定的话，则会随意一个端口.还有，不要设置1024以上的。原因如下
> Some systems limit access to ports below 1024 for security reasons.The methods for listening on privileged ports vary between systems, please refer to your system’s documentation for more information.

**max_connections**: {max_connections, Number|infinity}Number默认是1024.尽量不要infinity。
还可以通过`ranch:set_max_connections`设置.有时有个别进程是长链接，不希望它计入connections的计数。可以通过`ranch:remove_conections/1`移除1.
5. protocol_handler mod: echo_protocol
6. protocol options: []

### ranch:
listener的实现:
```
start_listener(tcp_echo, 1, ranch_tcp, [{port, 5555}], echo_protocol, []) ->
    ...
	ChildSpec = {{ranch_listener_sup, tcp_echo}, {ranch_listener_sup, start_link, [
		tcp_echo, 1, ranch_tcp, [{port, 5555}], echo_protocol, [] 
	]}, permanent, infinity, supervisor, [ranch_listener_sup]}.
	Res = supervisor:start_child(ranch_sup, ChildSpec),
	Socket = proplists:get_value(socket, [{port, 5555}]),
	case Res of
		{ok, Pid} when Socket =/= undefined ->
		    %% 此处更改socket的拥有者,暂不管
		  	Children = supervisor:which_children(Pid),
		  	{_, AcceptorsSup, _, _}
		  		= lists:keyfind(ranch_acceptors_sup, 1, Children),
		   catch Transport:controlling_process(Socket, AcceptorsSup);
		_ ->
			ok
	end,
	Res
	...
```

在未指定socket的情况，start_listener通过`supervisor:start_child/2`启动了`ranch_listener_sup`


### ranch_listener_sup:
listener_sup 仍然是个supervisor，它得到一个max_connections(默认1024）, 通知ranch_server储存max_connection,和Opts。
```
start_link(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
	MaxConns = proplists:get_value(max_connections, TransOpts, 1024),
	ranch_server:set_new_listener_opts(Ref, MaxConns, ProtoOpts),
	%% 注意此处没有注册名字
	supervisor:start_link(?MODULE, {
		Ref, NbAcceptors, Transport, TransOpts, Protocol
	}).
```

`ranch_server:`
```
handle_call({set_new_listener_opts, Ref, MaxConns, Opts}, _, State) ->
    %% 将数据存入rank_server ets.
	ets:insert(rank_server, {{max_conns, Ref}, MaxConns}),
	ets:insert(rank_server, {{opts, Ref}, Opts}),
	{reply, ok, State};
```

`ranch_listener_sup:`
```
init({tcp_echo, 1, ranch_tcp, [{port, 5555}], echo_protocol}) ->
	AckTimeout = proplists:get_value(ack_timeout, TransOpts, 5000),
	ConnType = proplists:get_value(connection_type, TransOpts, worker),
	Shutdown = proplists:get_value(shutdown, TransOpts, 5000),
	ChildSpecs = [
		{ranch_conns_sup, {ranch_conns_sup, start_link,
				[Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol]},
			permanent, infinity, supervisor, [ranch_conns_sup]},
		{ranch_acceptors_sup, {ranch_acceptors_sup, start_link,
				[Ref, NbAcceptors, Transport, TransOpts]},
			permanent, infinity, supervisor, [ranch_acceptors_sup]}
	],
	{ok, {{rest_for_one, 10, 10}, ChildSpecs}}.
```

listener_sup 启动 conns_sup和acceptors_sup,这两个都是supervisor 所以shutdown都设置成infinity
但是其中conns_sup是个自定义的supervisor，作者解释是为了优化create和accept connection。
> Ranch uses a custom supervisor for managing connections. This supervisor keeps track of the number of connections and handles connection limits directly. While it is heavily optimized to perform the task of creating connection processes for accepted connections, it is still following the OTP principles and the usual sys and supervisor calls will work on it as expected.

### Ranch_conns_sup:
```
start_link(Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol) ->
	proc_lib:start_link(?MODULE, init,
	[self(), Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol]).
	
init(Parent, Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol) ->
	process_flag(trap_exit, true),
	ok = ranch_server:set_connections_sup(Ref, self()),
	MaxConns = ranch_server:get_max_connections(Ref),
	Opts = ranch_server:get_protocol_options(Ref),
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	loop(#state{parent=Parent, ref=Ref, conn_type=ConnType,
		shutdown=Shutdown, transport=Transport, protocol=Protocol,
		opts=Opts, ack_timeout=AckTimeout, max_conns=MaxConns}, 0, 0, []).
```

```
14> rp(sys:get_status(ConnsSup)).
{status,<0.43.0>,
        {module,ranch_conns_sup},
        [[{'$ancestors',[<0.42.0>,ranch_sup,<0.34.0>]},
          {'$initial_call',{ranch_conns_sup,init,7}}],
         running,<0.42.0>,[],
         {{state,<0.42.0>,tcp_echo,worker,5000,ranch_tcp,
                 echo_protocol,[],5000,1024},
          0,0,[]}]}
```

上面使用`sys:get_status(Name|Pid)`得到ranch_conns_sup的#state{}。

### Ranch_acceptor_sup:
```
start_link(Ref, NbAcceptors, Transport, TransOpts) ->
	supervisor:start_link(?MODULE, [Ref, NbAcceptors, Transport, TransOpts]).

init([Ref, NbAcceptors, Transport, TransOpts]) ->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	LSocket = case proplists:get_value(socket, TransOpts) of
		undefined ->
			TransOpts2 = proplists:delete(ack_timeout,
				proplists:delete(connection_type,
				proplists:delete(max_connections,
				proplists:delete(shutdown,
				proplists:delete(socket, TransOpts))))),
			case Transport:listen(TransOpts2) of
				{ok, Socket} -> Socket;
				{error, Reason} -> listen_error(Ref, Transport, TransOpts2, Reason)
			end;
		Socket ->
			Socket
	end,
	{ok, Addr} = Transport:sockname(LSocket),
	ranch_server:set_addr(Ref, Addr),
	Procs = [
		{{acceptor, self(), N}, {ranch_acceptor, start_link, [
			LSocket, Transport, ConnsSup
		]}, permanent, brutal_kill, worker, []}
			|| N <- lists:seq(1, NbAcceptors)],
	{ok, {{one_for_one, 10, 10}, Procs}}.
```

acceptor_sup完成了得到一个ListenSocket, 然后启动N个acceptor进程，这些acceptor的loop等待`gen_tcp:accept` | `ssl:transport_accept`
`Transport:listen`的本质就是`gen_tcp:listen`或者`ssl:listen` 它返回一个ListenSocket
`Transport:sockname` :: `inet:sockname` | `ssl:sockname`
```
13> sys:get_status(AccepterSup).
{status,<0.44.0>,
        {module,gen_server},
        [[{'$ancestors',[<0.42.0>,ranch_sup,<0.34.0>]},
          {'$initial_call',{supervisor,ranch_acceptors_sup,1}}],
         running,<0.42.0>,[],
         [{header,"Status for generic server <0.44.0>"},
          {data,[{"Status",running},
                 {"Parent",<0.42.0>},
                 {"Logged events",[]}]},
          {data,[{"State",
                  {state,{<0.44.0>,ranch_acceptors_sup},
                         one_for_one,
                         [{child,<0.45.0>,
                                 {acceptor,<0.44.0>,1},
                                 {ranch_acceptor,start_link,
                                                 [#Port<0.917>,ranch_tcp,<0.43.0>]},
                                 permanent,brutal_kill,worker,[]}],
                         undefined,10,10,[],ranch_acceptors_sup,
                         [tcp_echo,1,ranch_tcp,[{port,5555}]]}}]}]]}
```

demo中只启动了一个acceptor
至此完了了所有进程的启动。等待connection进来。
![](/img/ranch_2.png)

### Acceptor:
acceptor的作用是`accept(ListenSocket) -> Socket.` 通过`Transport:controlling_process`,将端口控制交给`conns_sup`, 然后向conns_sup发送`{ranch_conns_sup, start_protocol, AcceptorPid, Socket}`

```
start_link(LSocket, Transport, ConnsSup) ->
	Pid = spawn_link(?MODULE, loop, [LSocket, Transport, ConnsSup]),
	{ok, Pid}.

-spec loop(inet:socket(), module(), pid()) -> no_return().
loop(LSocket, Transport, ConnsSup) ->
	_ = case Transport:accept(LSocket, infinity) of
		{ok, CSocket} ->
			case Transport:controlling_process(CSocket, ConnsSup) of
				ok ->
					ranch_conns_sup:start_protocol(ConnsSup, CSocket);
				{error, _} ->
					Transport:close(CSocket)
			end;
		{error, emfile} ->
			receive after 100 -> ok end;
		{error, Reason} when Reason =/= closed ->
			ok
	end,
	flush(),
	?MODULE:loop(LSocket, Transport, ConnsSup).
```

`ranch_conns_sup`:
```
start_protocol(SupPid, Socket) ->
	SupPid ! {?MODULE, start_protocol, self(), Socket},
	receive SupPid -> ok end.
```

```receive
		{?MODULE, start_protocol, To, Socket} ->
			try Protocol:start_link(Ref, Socket, Transport, Opts) of
				{ok, Pid} ->
					shoot(State, CurConns, NbChildren, Sleepers, To, Socket, Pid, Pid);
				{ok, SupPid, ProtocolPid} when ConnType =:= supervisor ->
					shoot(State, CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid);
				Ret ->
					To ! self(),
					error_logger:error_msg(
						"Ranch listener ~p connection process start failure; "
						"~p:start_link/4 returned: ~999999p~n",
						[Ref, Protocol, Ret]),
					Transport:close(Socket),
					loop(State, CurConns, NbChildren, Sleepers)
					......
```

```
shoot(State=#state{ref=Ref, transport=Transport, ack_timeout=AckTimeout, max_conns=MaxConns},
		CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid) ->
	case Transport:controlling_process(Socket, ProtocolPid) of
		ok ->
		    %% 通知echo_protocol进程控制权已转移。
			ProtocolPid ! {shoot, Ref, Transport, Socket, AckTimeout},
			put(SupPid, true),
			CurConns2 = CurConns + 1,
			if CurConns2 < MaxConns ->
					To ! self(),
					loop(State, CurConns2, NbChildren + 1, Sleepers);
				true ->
					loop(State, CurConns2, NbChildren + 1, [To|Sleepers])
			end;
		{error, _} ->
			Transport:close(Socket),
			%% Only kill the supervised pid, because the connection's pid,
			%% when different, is supposed to be sitting under it and linked.
			exit(SupPid, kill),
			loop(State, CurConns, NbChildren, Sleepers)
	end.
```

### ranch_protocol:
conns_sup在完成`controlling_process`后要通知protocol进程完成了转移。因为此时protocol的init还没有执行结束，一直在等着控制权转移。因为控制权没转移是不能进入loop的.spawn_link及时将自己的pid告诉了conns_sup.如果是gen_server等需要init结束才返回的进程需要特殊处理, 在下面分析.同时完成转移后conns_sup还给acceptor发送一条自己的pid，告诉acceptor转移完成。因为这之前acceptor一直处在receive状态，等待conns_sup完成工作。不完成他不会再次参与accept操作.
`echo_protocol:`
```
start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
  %% 必须调用 accept_ack，确保socket控制权
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport).
```

`ranch:`
```
accept_ack(Ref) ->
	receive {shoot, Ref, Transport, Socket, AckTimeout} ->
		Transport:accept_ack(Socket, AckTimeout)
	end.
```

链接套接字的逻辑进程都要有
1. -behavior(ranch_protocol). 定义了一个start_link/4 callback而已。
2. 在任何对端口的操作之前一定要确保执行过ranch:accept_ack(Ref)(确保端口控制权交给自己).在此之后可以运行ranch_tcp|ranch_ssl:setopts(Opt)完成自定义对端口的设置
如果同时他是个gen_server,gen_fsm等有自己的start_link, 会产生一个问题，因为init中放入ranch:accept_ack/1会形成死锁。(见上面，即`conns_sup` 在等待`ranch_protocol`进程的pid返回。而`ranch_protocol`在等待`conns_sup`将端口控制交给自己)见问题说明
[ranch_protocol_doc](https://github.com/ninenines/ranch/blob/master/doc/src/guide/protocols.asciidoc).
作者再上面给出了两种解决办法:
1. 在start_link 中用proc_lib:start_link代替gen_server:start_link,然后在init中主动调用proc_lib：init_ack通知父进程初始化完毕，然后调用ranch:accept_ack，再之后手动用gen_server:enter_loop进入循环。在之前这里分析过gen_server的初始化过程。
[gen_server和init](/2015/07/31/erlang-question-gen-server-and-init/)
2. 作者在init中返回timeout为0，然后通过handle_info(timeout...)调用ranch:accept_ack。实际上最好不要这么用，原因还是见上面的文章。具体剖析过为什么不能这么用。还是用 self()!timeout替代这种方法吧。
![](/img/ranch_3.png)
