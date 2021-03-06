title: erlang_mysql_driver 源码分析(一)
date: 2015-06-08 20:13:07
tags: erlang
categories: erlang
---

这两天抽时间看了看erlang_mysql_driver这个项目，用来建立管理服务器端mysql方面的处理。听说emysql也不错，过两天看看对比下。
这个项目主体有这么几个文件
> mysql.erl
mysql_auth.erl
mysql_conn.erl
mysql_recv.erl

<!--more-->
#mysql:start_link
主要接口在mysql里面。
mysql文件头有作者写的功能使用注释
> %%% Usage:
%%%
%%%
%%% Call one of the start-functions before any call to fetch/2
%%%
%%%   start_link(PoolId, Host, User, Password, Database)
%%%   start_link(PoolId, Host, Port, User, Password, Database)
%%%   start_link(PoolId, Host, User, Password, Database, LogFun)
%%%   start_link(PoolId, Host, Port, User, Password, Database, LogFun)
%%%
%%% (These functions also have non-linking coutnerparts.)


让我们在使用任何fetch之前先start_link,如果不需要link，同时提供了start接口。
好，找到start_link.
```
start_link(PoolId, Host, User, Password, Database) ->
    start_link(PoolId, Host, ?PORT, User, Password, Database).

start_link(PoolId, Host, Port, User, Password, Database) ->
    start_link(PoolId, Host, Port, User, Password, Database, undefined,
	       undefined).

start_link(PoolId, Host, undefined, User, Password, Database, LogFun) ->
    start_link(PoolId, Host, ?PORT, User, Password, Database, LogFun,
	       undefined);
start_link(PoolId, Host, Port, User, Password, Database, LogFun) ->
    start_link(PoolId, Host, Port, User, Password, Database, LogFun,
	       undefined).

start_link(PoolId, Host, undefined, User, Password, Database, LogFun,
	   Encoding) ->
    start1(PoolId, Host, ?PORT, User, Password, Database, LogFun, Encoding,
	   start_link);
start_link(PoolId, Host, Port, User, Password, Database, LogFun, Encoding) ->
    start1(PoolId, Host, Port, User, Password, Database, LogFun, Encoding,
	   start_link).
```

start与start_link大致相同，只不过最后参数是start不是start_link
```
...
start(PoolId, Host, undefined, User, Password, Database, LogFun, Encoding) ->
    start1(PoolId, Host, ?PORT, User, Password, Database, LogFun, Encoding,
	   start);
start(PoolId, Host, Port, User, Password, Database, LogFun, Encoding) ->
    start1(PoolId, Host, Port, User, Password, Database, LogFun, Encoding,
	   start).
```

其中
> PoolId: 这个进程的id，随便起，比如my_sql_game.
Host: 目标ip, 本机写"127.0.0.1"
Port: 端口号，不改的话默认是3306
User: 数据库的用户名：比如 youthy
Password：密码
Database:数据库名
LogFun:日志输出函数，如果undefined则会用默认的输出
Encoding:编码方式，我们这里用utf8.

然后转入start1这个函数
```
start1(PoolId, Host, Port, User, Password, Database, LogFun, Encoding,
       StartFunc) ->
    crypto:start(),
    gen_server:StartFunc(
      {local, ?SERVER}, ?MODULE,
      [PoolId, Host, Port, User, Password, Database, LogFun, Encoding], []).
```

crypto是之后需要一些加密算法， 比如md5，sha等，使用前必须先start。
而后就是再熟悉不过的gen_server,这里面根据方式不同，分别调用start_link, start,然后找到init函数
```
init([PoolId, Host, Port, User, Password, Database, LogFun, Encoding]) ->
    erlang:process_flag(priority, high),
    LogFun1 = if LogFun == undefined -> fun log/4; true -> LogFun end,
    case mysql_conn:start(Host, Port, User, Password, Database, LogFun1,
			  Encoding, PoolId) of
	{ok, ConnPid} ->
	    Conn = new_conn(PoolId, ConnPid, true, Host, Port, User, Password,
			    Database, Encoding),
	    State = #state{log_fun = LogFun1},
	    {ok, add_conn(Conn, State)};
	{error, Reason} ->
	    ?Log(LogFun1, error,
		 "failed starting first MySQL connection handler, "
		 "exiting"),
	    {stop, {error, Reason}}
    end.
```

首先用process_flag将优先级设为高。之后调用mysql_conn:start/8,等会再进去看它内部，我们只知道它返回一个ConnPid,或者出错，然后将这个Pid用new_conn生成一个Conn。然后用add_conn将Conn加入State中，返回，至此这个gen_server创建完毕。名字是"my_sql_game"（随便取）。我们看一下new_conn和add_conn这两个函数
```
new_conn(PoolId, ConnPid, Reconnect, Host, Port, User, Password, Database,
	 Encoding) ->
    case Reconnect of
	true ->
	    #conn{pool_id = PoolId,
		  pid = ConnPid,
		  reconnect = true,
		  host = Host,
		  port = Port,
		  user = User,
		  password = Password,
		  database = Database,
		  encoding = Encoding
		 };
	false ->                        
	    #conn{pool_id = PoolId,
		  pid = ConnPid,
		  reconnect = false}
    end.
---------
add_conn(Conn, State) ->
    Pid = Conn#conn.pid,
    erlang:monitor(process, Conn#conn.pid),
    PoolId = Conn#conn.pool_id,
    ConnPools = State#state.conn_pools,
    NewPool = 
	case gb_trees:lookup(PoolId, ConnPools) of
	    none ->
		{[Conn],[]};
	    {value, {Unused, Used}} ->
		{[Conn | Unused], Used}
	end,
    State#state{conn_pools =
		gb_trees:enter(PoolId, NewPool,
			       ConnPools),
		pids_pools = gb_trees:enter(Pid, PoolId,
					    State#state.pids_pools)}.
```

这里涉及了两个结构
```
-record(conn, {
	  pool_id,      %% atom(), the pool's id
	  pid,          %% pid(), mysql_conn process	 
	  reconnect,	%% true | false, should mysql_dispatcher try
                        
	  host,		%% string()
	  port,		%% integer()
	  user,		%% string()
	  password,	%% string()
	  database,	%% string()
	  encoding
	 }).
	 
-record(state, {
	
	  conn_pools = gb_trees:empty(), 


	  pids_pools = gb_trees:empty(),                               

	  log_fun,	

	  prepares = gb_trees:empty()
	 }).
```

conn这个结构大部分就是把函数参数存了起来，其中只有pid需要注意下，它就是mysql_conn:start返回的Pid，new_conn这个函数将参数转成了#conn这个record，然后通过add_conn将#conn存入了#state的conn_pools.字面理解就是链接池。值得注意的是my_sql_game这个gen_server的state用了3个gb_trees这个存储结构。也就是二叉查找树。erlang:monitor(process, Conn#conn.pid).
> monitor(Type, Item) -> MonitorRef
Types:
Type = process
Item = pid() | {RegName, Node} | RegName
 RegName = atom()
 Node = node()
MonitorRef = reference()

目前Type只接受process这个参数，这个函数字面理解就是监控，让这个gen_server监控之前建立的conn进程，被监控进程如果崩溃了，一条{'DOWN', MonitorRef, Type, Object，Info}
消息会发往监控者，这时可以被handle_info处理。
```
handle_info({'DOWN', _MonitorRef, process, Pid, Info}, State) ->
    LogFun = State#state.log_fun,
    case remove_conn(Pid, State) of
	{ok, Conn, NewState} ->
	    LogLevel = case Info of
			   normal -> normal;
			   _ -> error
		       end,
	    ?Log2(LogFun, LogLevel,
		"connection pid ~p exited : ~p", [Pid, Info]),
	    case Conn#conn.reconnect of
		true ->
		    start_reconnect(Conn, LogFun);
		false ->
		    ok
	    end,
	    {noreply, NewState};
	error ->
	    ?Log2(LogFun, error,
		  "received 'DOWN' signal from pid ~p not in my list", [Pid]),
	    {noreply, State}
    end;
```

可以看出主要是讲state里面的conn_pool里面把这个崩掉的进程conn移除，然后如果这个链接conn的属性是reconnect，那么会重新start_reconnect。先不展开，以机会再说，要不init的过程就跑远了。
回到erlang:monitor.(还有个对应的函数是demonitor，可以取消监控)
```
NewPool = 
	case gb_trees:lookup(PoolId, ConnPools) of
	    none ->
		{[Conn],[]};
	    {value, {Unused, Used}} ->
		{[Conn | Unused], Used}
	end,
```

从ConnPool中找到PoolId的结点，由case可见这个结点存的是{Unused， Used}的形式。
gb_tree的结构是{Size, Tree}.Size表示这个tree有多少个结点。tree的结构是{key, Value, Smaller, Biger}.

```
1> gb_trees:empty().
{0,nil}
2> gb_trees:insert(5, v0, v(1)).
{1,{5,v0,nil,nil}}
3> gb_trees:insert(8, v1, v(2)).
{2,{5,v0,nil,{8,v1,nil,nil}}}
4> gb_trees:insert(3, v2, v(3)).
{3,{5,v0,{3,v2,nil,nil},{8,v1,nil,nil}}}
5> gb_trees:insert(7, v3, v(4)).
{4,{5,v0,{3,v2,nil,nil},{8,v1,{7,v3,nil,nil},nil}}}
6> gb_trees:insert(1, v4, v(5)).
{5,
 {5,v0,{3,v2,{1,v4,nil,nil},nil},{8,v1,{7,v3,nil,nil},nil}}}
```

上面演示了一个gb_tree的建立，gb_tree的大小判断是基于key的。二叉树数据结构都学过。
我们可以看一下state里面的结构究竟是什么样子
```
mysql:613: state:{state,
                     {1,
                      {my_sql_game,
                          {[{conn,my_sql_game,<0.62.0>,true,"127.0.0.1",3306,
                                "aries","arig","aries_game",utf8},
                            {conn,my_sql_game,<0.58.0>,true,"127.0.0.1",3306,
                                "aries","arig","aries_game",utf8},
                            {conn,my_sql_game,<0.54.0>,true,"127.0.0.1",3306,
                                "aries","arig","aries_game",utf8}],
                           [{conn,my_sql_game,<0.41.0>,true,"127.0.0.1",3306,
                                "aries","arig","aries_game",utf8}]},
                          nil,nil}},
                     {4,
                      {<0.41.0>,my_sql_game,nil,
                       {<0.54.0>,my_sql_game,nil,
                        {<0.58.0>,my_sql_game,nil,
                         {<0.62.0>,my_sql_game,nil,nil}}}}},
                     #Fun<mysql.0.74618748>,
                     {0,nil}}
```

可见我们my_sql_game的conn_pool的size始终为1，
因为我们在add_conn函数中的最后总是将conn enter进poolId为my_sql_game的链接池，之后又以pid为key，enter进pid_pool,因为pid不是唯一的，所以pid_pool的size会不断变大。至此我们了解了my_sql_game这个gen_server的创建过程。下篇看一下之前没看到部分。
相关参考
> [gb_trees(坚强2002博客）](http://www.cnblogs.com/me-sa/archive/2012/06/23/erlang-gb_trees.html)
[A Short Visit to Common Data Structures(推荐！)](http://learnyousomeerlang.com/a-short-visit-to-common-data-structures)
[gb_trees(官方doc)](http://www.erlang.org/doc/man/gb_trees.html)
