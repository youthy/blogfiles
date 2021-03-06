title: erlang_mysql_driver　源码分析(二)
date: 2015-06-09 17:21:15
tags: erlang
categories: erlang
---

#mysql_conn:start
回到mysql:start_link这个最开始这个地方

```
 LogFun1 = if LogFun == undefined -> fun log/4; true -> LogFun end,
    case mysql_conn:start(Host, Port, User, Password, Database, LogFun1,
			  Encoding, PoolId) of
	{ok, ConnPid} ->
```

<!--more-->
起初为了不断开gen_server的创建，我们没有进入mysql_conn里面。现在可以进去一窥究竟啦。
```
start(Host, Port, User, Password, Database, LogFun, Encoding, PoolId) ->
    ConnPid = self(),
    Pid = spawn(fun () ->
			init(Host, Port, User, Password, Database,
			     LogFun, Encoding, PoolId, ConnPid)
		end),
    post_start(Pid, LogFun).
    --------------------
post_start(Pid, LogFun) ->
    receive
	{mysql_conn, Pid, ok} ->
	    {ok, Pid};
	{mysql_conn, Pid, {error, Reason}} ->
	    {error, Reason};
	Unknown ->
	    ?Log2(LogFun, error,
		 "received unknown signal, exiting: ~p", [Unknown]),
	    {error, "unknown signal received"}
    after 5000 ->
	    {error, "timed out"}
    end.
```

这里作者将post_start抽出来了，因为与start类似还有个start_link也需要这块代码，但是值得注意的是
post_start里面的receive仍然是由mysql:start_link生成的my_sql_game这个gen_server 在调用，而且` ConnPid = self()`这句代码很有歧义，因为这个ConnPid与mysql中的`{ok, ConnPid}`明显不是同一个意思。这里的ConnPid实际就是gen_server的Pid，用来传给init作为父进程参数，在子进程可以给父进程发消息,我觉得把这段post_start放到mysql中更容易理解一点。
这里我们可以看出来，start函数spawn一个mysql_conn:init这个进程，然后等待消息，如果成功的话，会返回{ok, Pid}，这个Pid是真正意义的ConnPid，用来生成#conn加入my_sql_game的#state里面的。其他情况会返回{error, Reason}.
然后看一下init
```
init(Host, Port, User, Password, Database, LogFun, Encoding, PoolId, Parent) ->
    case mysql_recv:start_link(Host, Port, LogFun, self()) of
	{ok, RecvPid, Sock} ->
	    case mysql_init(Sock, RecvPid, User, Password, LogFun) of
		{ok, Version} ->
		    Db = iolist_to_binary(Database),
		    case do_query(Sock, RecvPid, LogFun,
				  <<"use ", Db/binary>>,
				  Version) of
			{error, MySQLRes} ->
			    ?Log2(LogFun, error,
				 "mysql_conn: Failed changing to database "
				 "~p : ~p",
				 [Database,
				  mysql:get_result_reason(MySQLRes)]),
			    Parent ! {mysql_conn, self(),
				      {error, failed_changing_database}};

			%% ResultType: data | updated
			{_ResultType, _MySQLRes} ->
			    Parent ! {mysql_conn, self(), ok},
			    case Encoding of
				undefined -> undefined;
				_ ->
				    EncodingBinary = list_to_binary(atom_to_list(Encoding)),
				    do_query(Sock, RecvPid, LogFun,
					     <<"set names '", EncodingBinary/binary, "'">>,
					     Version)
			    end,
			    State = #state{mysql_version=Version,
					   recv_pid = RecvPid,
					   socket   = Sock,
					   log_fun  = LogFun,
					   pool_id  = PoolId,
					   data     = <<>>
					  },
			    loop(State)
		    end;
		{error, _Reason} ->
		    Parent ! {mysql_conn, self(), {error, login_failed}}
	    end;
	E ->
	    ?Log2(LogFun, error,
		 "failed connecting to ~p:~p : ~p",
		 [Host, Port, E]),
	    Parent ! {mysql_conn, self(), {error, connect_failed}}
    end.
```

这个init比较长，而且里面又调了mysql_recv:start_link，之前之所以没有直接分析这里面也是这个原因，在start_link里面重复调用各个模块的start_link很容易绕进去。
同样，我们根据case语句，知道mysql_recv:start_link建了一个新的进程，并且返回RecvPid和一个Sock，可知创建进程的同时监听了某个端口。如果没有成功会返回{error， Reason}

之后进入mysql_init函数。我们先不要进入mysql_init，这个函数完成了用户名和密码的认证，认证成功会返回{ok, Version}

接下来要做的是
` case do_query(Sock, RecvPid, LogFun,
				  <<"use ", Db/binary>>,
				  Version) of`
通知端口我们要使用哪个数据库，打印出来是这段文字
`fetch <<"use aries_game">>`
返回结果{error, MysqlRes}或者{ResultType, MysqlRes}.
MysqlRes是一个record
> -record(mysql_result,
	{fieldinfo=[],
	 rows=[],
	 affectedrows=0,
	 error=""}).

在do_query的里面，将端口返回来的结果构造成#mysql_result的样子，他包含了需要的rows，或者操作所影响的affetedrows，fieldinfo以及如果出错的错误信息error.
如果返回{error, MysqlRes}, 我们调用接口mysql:get_result_reason(MysqlRes)获取错误原因。
```
get_result_reason(#mysql_result{error=Reason}) ->
    Reason.
```

并通知父进程出错了
```
Parent ! {mysql_conn, self(), {error, failed_changing_database}};
```

相反
```
{_ResultType, _MySQLRes} ->
			    Parent ! {mysql_conn, self(), ok},
```

如果没有出错，我们就告诉父进程ok。
还记得post_start里面的
```
post_start(Pid, LogFun) ->
    receive
	{mysql_conn, Pid, ok} ->
	    {ok, Pid};
	{mysql_conn, Pid, {error, Reason}} ->
	    {error, Reason};
```

这就是receive我们这些消息的地方。
后面的
```
  EncodingBinary = list_to_binary(atom_to_list(Encoding)),
				    do_query(Sock, RecvPid, LogFun,
					     <<"set names '", EncodingBinary/binary, "'">>,
					     Version)
```

用来设定我们的编码方式，目前用的utf8.
下面了解下do_query的细节，因为之后我们要做的查询，插入等操作也经过它
```
do_query(Sock, RecvPid, LogFun, Query, Version) ->
    Query1 = iolist_to_binary(Query),
    ?Log2(LogFun, debug, "fetch ~p (id ~p)", [Query1,RecvPid]),
    Packet =  <<?MYSQL_QUERY_OP, Query1/binary>>,
    case do_send(Sock, Packet, 0, LogFun) of
	ok ->
	    get_query_response(LogFun,RecvPid,
				    Version);
	{error, Reason} ->
	    Msg = io_lib:format("Failed sending data "
				"on socket : ~p",
				[Reason]),
	    {error, Msg}
    end.
```

Query是我们的sql语句，比如上面的<<<"use aries_game">>,
Query1使我们给Query加上一个字节的3(?MYSQL_QUERY_OP是3）.
do_send将packet打包并发送，头三个字节是packet大小，第4个字节是序列号，之后是packet内容，如下
```
do_send(Sock, Packet, SeqNum, _LogFun) when is_binary(Packet), is_integer(SeqNum) ->
    Data = <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>,
    gen_tcp:send(Sock, Data).
```

这时候向端口发送了请求,get_query_response等待回应。
```
get_query_response(LogFun, RecvPid, Version) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, <<Fieldcount:8, Rest/binary>>, _} ->
	    case Fieldcount of
		0 ->
		    %% No Tabular data
            <<AffectedRows:8, _Rest2/binary>> = Rest,
            ?Log2(LogFun, debug, "updated ~p", [AffectedRows]),
		    {updated, #mysql_result{affectedrows=AffectedRows}};
		255 ->
		    <<_Code:16/little, Message/binary>>  = Rest,
		    {error, #mysql_result{error=Message}};
		_ ->
		    %% Tabular data received
		    case get_fields(LogFun, RecvPid, [], Version) of
			{ok, Fields} ->
			    case get_rows(Fields, LogFun, RecvPid, []) of
				{ok, Rows} ->
                    ?Log2(LogFun, debug, "data: field:~p, rows:~p", [Fields, Rows]),
                    {data, #mysql_result{fieldinfo=Fields,
                            rows=Rows}};
				{error, Reason} ->
				    {error, #mysql_result{error=Reason}}
			    end;
			{error, Reason} ->
			    {error, #mysql_result{error=Reason}}
		    end
	    end;
	{error, Reason} ->
	    {error, #mysql_result{error=Reason}}
    end.
```

第一句do_recv主要用来对第三个参数帧序列号的不同做匹配，这里是undefined，意在接受任何帧，如果指定一个帧号A，那么do_recv只接受A+1的消息。如下
```
 do_recv(LogFun, RecvPid, SeqNum)  when is_function(LogFun);
				       LogFun == undefined,
				       SeqNum == undefined ->
    receive
        {mysql_recv, RecvPid, data, Packet, Num} ->
	    {ok, Packet, Num};
	{mysql_recv, RecvPid, closed, _E} ->
	    {error, "mysql_recv: socket was closed"}
    end;
do_recv(LogFun, RecvPid, SeqNum) when is_function(LogFun);
				      LogFun == undefined,
				      is_integer(SeqNum) ->
    ResponseNum = SeqNum + 1,
    receive
        {mysql_recv, RecvPid, data, Packet, ResponseNum} ->
	    {ok, Packet, ResponseNum};
	{mysql_recv, RecvPid, closed, _E} ->
	    {error, "mysql_recv: socket was closed"}
    end.
```

这个函数将端口返回的Packet和帧序号返回，或者帧序号+1返回。
回到`case do_recv(LogFun, RecvPid, undefined) of`
接下来是对结果的匹配，Fieldcount为0表示执行的是update操作，而不是请求某些数据。如果为255则表示出错，返回{error, #mysql_result{error = Message}}.
其他数值时，会执行get_field().
```
%% Support for MySQL 4.1.x and 5.x:
get_fields(LogFun, RecvPid, Res, ?MYSQL_4_1) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, Packet, _Num} ->
        ?Log2(LogFun, debug, "get_field: packet ~p", [Packet]),
	    case Packet of
		<<254:8>> ->
		    {ok, lists:reverse(Res)};
		<<254:8, Rest/binary>> when size(Rest) < 8 ->
		    {ok, lists:reverse(Res)};
		_ ->
		    {_Catalog, Rest} = get_with_length(Packet),
		    {_Database, Rest2} = get_with_length(Rest),
		    {Table, Rest3} = get_with_length(Rest2),
		    %% OrgTable is the real table name if Table is an alias
		    {_OrgTable, Rest4} = get_with_length(Rest3),
		    {Field, Rest5} = get_with_length(Rest4),
		    %% OrgField is the real field name if Field is an alias
		    {_OrgField, Rest6} = get_with_length(Rest5),

		    <<_Metadata:8/little, _Charset:16/little,
		     Length:32/little, Type:8/little,
		     _Flags:16/little, _Decimals:8/little,
		     _Rest7/binary>> = Rest6,
		    
		    This = {Table,
			    Field,
			    Length,
			    get_field_datatype(Type)},
		    get_fields(LogFun, RecvPid, [This | Res], ?MYSQL_4_1)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.
```

这里提供了两个版本的get_field,我的mysql是5.5所以会匹配到这个函数上。
get_field一开始仍然是do_recv，用来提取一个Packet。case Packet告诉我们这个包第一个字节是254的时候表示结果已经全部告诉我们了，这时候会将Res（result）翻转，返回。
get_with_length用来将Packet切割，用来得到Table， Field， Length等参数。
```
get_with_length(<<251:8, Rest/binary>>) ->
    {null, Rest};
get_with_length(<<252:8, Length:16/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<253:8, Length:24/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<254:8, Length:64/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<Length:8, Rest/binary>>) when Length < 251 ->
    split_binary(Rest, Length).
```

这个函数的意思大致可以看出来，如果第一个字节是251，就直接返回null和Rest，如果第一个字节小于251，那么第一个字节表示长度，将Rest分割成{Value， Rest2}，Value是我们需要的，对应长度的值，如果第一个字节大于251，分不同的情况，接下来的2个4个或者8个字节表示长度，将Rest分割。举个例子
```
mysql:fetch(mysql_game_pool, "select name from player where id = 301").

=INFO REPORT==== 9-Jun-2015::16:43:29 ===
I(<0.94.0>:mysql_conn:426) : fetch <<"select name from player where id = 301">> (id <0.95.0>)

=INFO REPORT==== 9-Jun-2015::16:43:29 ===
I(<0.94.0>:mysql_conn:726) : get_field: packet <<3,100,101,102,10,97,114,105,
                                                 101,115,95,103,97,109,101,6,
                                                 112,108,97,121,101,114,6,112,
                                                 108,97,121,101,114,4,110,97,
                                                 109,101,4,110,97,109,101,12,
                                                 33,0,150,0,0,0,253,5,64,0,0,0>>

=INFO REPORT==== 9-Jun-2015::16:43:29 ===
I(<0.94.0>:mysql_conn:726) : get_field: packet <<254,0,0,2,0>>
{data,{mysql_result,[{<<"player">>,<<"name">>,150,
                      'VAR_STRING'}],
                    [[<<"aaaa">>]],
                    0,[]}}

=INFO REPORT==== 9-Jun-2015::16:43:29 ===
I(<0.94.0>:mysql_conn:770) : get_rows: packet <<4,97,97,97,97>>

=INFO REPORT==== 9-Jun-2015::16:43:29 ===
I(<0.94.0>:mysql_conn:770) : get_rows: packet <<254,0,0,2,0>>
(aries_game@192.168.1.85)2> 
=INFO REPORT==== 9-Jun-2015::16:43:29 ===
I(<0.94.0>:mysql_conn:667) : data: field:[{<<"player">>,<<"name">>,150,
                                           'VAR_STRING'}], rows:[[<<"aaaa">>]]

```

我们执行了一个sql语句，从player表中选出id为301的玩家的名字。
端口回复给我们的是get_field后面打印的二进制串。
  1. 第一个字节是3，不是254。走get_with_length拆分。
  2. 将100,101,102提出，为CataLog,这个参数我们不需要。
  3. 10表示接下来取10个字节，一直到109，101.位Database，我们也不需要
  4. 接下来6，表示6个字节，<<112,108,97,121,101,114>>提取为Table，也就是我们的表名，我们在shell中打一下可以看到实际上就是<<"player">>。表示我们是从player表取得。
  5. 接下来6个字节和上面一下，我们不需要。
  6. 下面4个字节<<110,97,109,101>>,就是<<"name">>,表示字段名。
  7. 后面4个字节以上面一下，不需要。
  8. 从12开始一直到最后我们只需要<<150,0,0,0>>这四个表示长度,注意是小段存储，即长150，253，表示Type.
  9. 紧着着第一个packet到达，由于是254，告诉我们这个语句结果已返回。
这个时候我们只知道是player表的name字段，接下来get_rows将得到具体结果。
```
get_rows(Fields, LogFun, RecvPid, Res) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, Packet, _Num} ->
        ?Log2(LogFun, debug, "get_rows: packet ~p", [Packet]),
	    case Packet of
		<<254:8, Rest/binary>> when size(Rest) < 8 ->
		    {ok, lists:reverse(Res)};
		_ ->
		    {ok, This} = get_row(Fields, Packet, []),
		    get_rows(Fields, LogFun, RecvPid, [This | Res])
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%% part of get_rows/4
get_row([], _Data, Res) ->
    {ok, lists:reverse(Res)};
get_row([Field | OtherFields], Data, Res) ->
    {Col, Rest} = get_with_length(Data),
    This = case Col of
	       null ->
		   undefined;
	       _ ->
		   convert_type(Col, element(4, Field))
	   end,
    get_row(OtherFields, Rest, [This | Res]).
```
其中Field为[{<<"player">>,<<"name">>,150,'VAR_STRING'}],
与上面get_field大致相同，row的这个packet很简单，<<4,97,97,97,97>>，后面的4个字节就是我们要的结果就是<<"aaaa">>.然后convert_type将结果转为对应的形式
convert_type不贴了，ｈｅｘｏ转码会有问题．
我们的类型是VAR_STRING，直接原样返回。
这样回到get_query_response，我们得到了需要的结果{data, #mysql_result{}}这种形式。
在往上回到init处，我们执行完<<"use XXX(数据库名)">>, 返回成功后，执行<<"set names utf8">>,
最后构造State
```
State = #state{mysql_version=Version,
					   recv_pid = RecvPid,
					   socket   = Sock,
					   log_fun  = LogFun,
					   pool_id  = PoolId,
					   data     = <<>>
					  },
			    loop(State)
```

并开始loop。至此这个mysql_conn进程创建完毕。
