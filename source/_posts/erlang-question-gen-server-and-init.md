title: '[erlang_question]gen_server and init'
date: 2015-07-31 16:11:50
tags: erlang
---




<!-- toc -->

<!--more-->
## 小技巧?
这两天复习了一下[Erlang and OTP in action](http://zh.scribd.com/doc/87376094/Erlang-and-OTP-in-Action#scribd)
其中讲到gen_server的时候，作者用了个小技巧。如下。
![](http://storage4.static.itmages.com/i/15/0731/h_1438311976_3257184_46c6e8b7c0.png)

![](http://storage1.static.itmages.com/i/15/0731/h_1438312022_8979746_d612eb6d7f.png)

![](http://storage4.static.itmages.com/i/15/0731/h_1438311785_2601988_b290e0c784.png)
> 第三章图片的引用里handle_call应该是handle_info,此处有错误

作者的意思时如果在init的返回中设置了timeout参数为0，会在init结束后会立即向自己发送一个timeout参数。并且由handle_info(timeout...)处理。意思是可以让一些耗时高的初始化操作在handle_info(timeout。。）里面处理，而让init尽快返回。
**本以为是个不错的小技巧，结果却有些隐患**

## Question
这两天的mailist正好有人遇到了这个问题。
[[erlang_question]gen_server and init](http://erlang.org/pipermail/erlang-questions/2015-July/085314.html)
问题如下：
>***Matthew Evans***:
Hi,
We have used this pattern in gen_servers for a long time now:
init(_) ->   %% Some stuff   {ok, #state{}, 0}.
%%%%%%
handle_info(timeout, State) ->    %% Init stuff    {noreply,State}
Basically the idea is to prevent init from blocking and to have init/1 return with a timeout of 0 causing an immediate timeout message to be invoked in the handle_info.
Here's what's odd. Sometimes this timeout does not fire. It doesn't appear that any message is getting sent, but I would imagine that since it's a registered gen_server there is no way that can happen.
Does anyone have any ideas?
We are running vsn 17.1, and ntp is enabled on the host (Linux).
Thanks 		 	   		  

提问者与作者的思路一样，想要在timeout里面做些初始化操作。然而却没想到timeout并没有触发。
以下摘取一些回答。
## Answers:
### ***Sergej Jurečko***:
>If something is in the message queue before init is done executing, that timeout will never get called. Timeout means execute only if nothing else happens during. You should be using self() ! timeout 
如果在init没有完成之前消息队列里面存在消息，timeout将永远不会调用。应该使用self()!timeout这种方式

### ***Loïc Hoguin***(cowboy 作者）
>This and sending yourself a message is a bad idea. It will usually work, 
until it doesn't, and you will have a very hard time figuring out why.
Instead, start the process using proc_lib:spawn_link or 
proc_lib:start_link (depending on whether it needs to be synchronous or 
not), then perform your initialization (calling proc_lib:init_ack where 
appropriate), and finally calling gen_server:enter_loop.
What this gives you is pretty much the ability to customize the 
initialization of your gen_server process.
This is the correct way to do it. Your solution can fail if you receive 
a message. Sending yourself a message is subject to race conditions 
where you receive a message before you could init.
大体意思说这个方式是bad idea。应该使用proc_lib:spawn_link, proc_lib:start_link, proc_lib:init_ack, gen_server:enter_loop等。

### ***Max Lapshin***
>Loic is right, but you should understand that you can receive message
before your  "self() ! init" message only if you explicitly tell your pid
someone in init()  because before the end of init your pid is unknown to
others.
Loic是对的～但是你应该明白除非你在init的过程中将自己的pid告诉其他进程才会在init返回之前接到消息，因为init没有完成之前，pid对其他是未知的。

### ***Loïc Hoguin***
>(Had problems pasting so hope it's all OK!)

>I'm not sure what you wrote there but I can give you two scenarios where 
it can fail off the top of my head. The first is very unlikely and can 
only fail if you use the 0 timeout, while the second is actually much 
easier to observe and can fail with both methods:

>P1 calls start_link
P2 init (returns 0 timeout)
P2 yields before calling receive
P1 returns from start_link and sends P2 a message
P2 receives message

>And:

>P1 calls start_link
P2 in init subscribes to some kind of pubsub PS
PS sends P2 message(s)
P2 returns from init and receive those messages

>I'm not sure why your tool doesn't catch the first case, Concuerror 
*definitely does*, and without needing to write all this weird code too. :-)

>Here is the module:
```
-module(z).
-behaviour(gen_server).

-export([start_link/0]).

%% Operational API
-export([read/0]).

%% gen_server API
-export([
          init/1,
          handle_cast/2,
          handle_call/3,
          terminate/2,
          code_change/3,
          handle_info/2,
		 test/0
]).

test() ->
	start_link(),
	ready = read(),
	stop().

%% API
start_link() ->
     gen_server:start({local, ?MODULE}, ?MODULE, [], []).

read() ->
     gen_server:call(?MODULE, read).

stop() ->
	gen_server:call(?MODULE, stop).

%% Callbacks
init([]) ->
     {ok, initializing, 0}.

handle_call(read, _From, State) ->
     {reply, State, State};
handle_call(stop, _, State) ->
	{stop, normal, ok, stop}.

handle_cast(_M, State) ->
     {noreply, State}.

handle_info(timeout, _State) ->
     {noreply, ready}.

terminate(_How, _State) ->
     ok.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.
```
（注：handle_call(stop, _, State)的State应该是_State, 否则编译不过).

## Test
Loic给出了一个module，用concuerror进行了测试，这个模块的init中指定timeout为0，然后handle_info(timeout。。。）并没有触发。
concuerror是一个用来测试erlang中一些看起来没有问题的程序，但是在并行情况下可能发生的问题的工具。
参考
[官网](http://concuerror.com/)
[concuerror tutorial](http://concuerror.com/tutorials/poolboy-example/)
[github](https://github.com/parapluu/Concuerror)
以下是我运行的结果。
```
youthy@youthy:~/code/Concuerror$ ./concuerror -f "../etest/z.erl" -m z -t test --after_timeout 1000 
concuerror: WARNING: file ../etest/z.erl shadows the default ./z.beam
Concuerror started at 31 Jul 2015 07:21:55
Writing results in concuerror_report.txt

Info: Instrumented z
Info: Instrumented io_lib
Info: Instrumented gen_server
Info: Instrumented gen
Info: Instrumented proc_lib
Info: Instrumented erlang
Info: Instrumented init
Info: Instrumented sys
Info: You can see pairs of racing instructions (in the report and --graph) with '--show_races true'
Error: Stop testing on first error. (Check '-h keep_going').

Done! (Exit status: warning)
  Summary: 1 errors, 4/4 interleavings explored
youthy@youthy:~/code/Concuerror$ vim concuerror_report.txt 
```

concuerror_report.txt:
```
Erroneous interleaving 1:
* At step 23 process P exited abnormally
    Reason:
      {{badmatch,initializing},
       [{z,test,0,[{file,"../etest/z.erl"},{line,22}]}]}
    Stacktrace:
      [{z,test,0,[{file,"../etest/z.erl"},{line,22}]}]
* Blocked at a 'receive' (when all other processes have exited):
    P.1 in gen_server.erl line 348
--------------------------------------------------------------------------------

Interleaving info:
   1: P: undefined = erlang:whereis(z)
    in gen.erl line 277
   2: P: [] = erlang:process_info(P, registered_name)
    in proc_lib.erl line 648
   3: P: P.1 = erlang:spawn_opt({proc_lib,init_p,[P,[],gen,init_it,[gen_server,P,self,{local,z},z,[],[]]],[]})
    in erlang.erl line 249
   4: P.1: undefined = erlang:put('$ancestors', [P])
    in proc_lib.erl line 221
   5: P.1: undefined = erlang:put('$initial_call', {z,init,1})
    in proc_lib.erl line 222
   6: P.1: true = erlang:register(z, P.1)
    in gen.erl line 280
   7: P.1: {P.1,{get_argument,generic_debug}} = init ! {P.1,{get_argument,generic_debug}}
    in init.erl line 145
   8: Message ({P.1,{get_argument,generic_debug}}) from P.1 reaches init
   9: Message ({init,error}) from init reaches P.1
  10: P.1: receives message ({init,error})
    in init.erl line 146
  11: P.1: {ack,P.1,{ok,P.1}} = P ! {ack,P.1,{ok,P.1}}
    in proc_lib.erl line 348
  12: Message ({ack,P.1,{ok,P.1}}) from P.1 reaches P
  13: P: receives message ({ack,P.1,{ok,P.1}})
    in proc_lib.erl line 321
  14: P: P.1 = erlang:whereis(z)
    in gen.erl line 154
  15: P: #Ref<0.0.0.176> = erlang:monitor(process, P.1)
    in gen.erl line 204
  16: P: {'$gen_call',{P,#Ref<0.0.0.176>},read} = erlang:send(P.1, {'$gen_call',{P,#Ref<0.0.0.176>},read}, [noconnect])
    in gen.erl line 215
  17: Message ({'$gen_call',{P,#Ref<0.0.0.176>},read}) from P reaches P.1
  18: P.1: receives message ({'$gen_call',{P,#Ref<0.0.0.176>},read})
    in gen_server.erl line 348
  19: P.1: {#Ref<0.0.0.176>,initializing} = P ! {#Ref<0.0.0.176>,initializing}
    in gen_server.erl line 214
  20: Message ({#Ref<0.0.0.176>,initializing}) from P.1 reaches P
  21: P: receives message ({#Ref<0.0.0.176>,initializing})
    in gen.erl line 217
  22: P: true = erlang:demonitor(#Ref<0.0.0.176>, [flush])
    in gen.erl line 219
  23: P: exits abnormally ({{badmatch,initializing},[{z,test,0,[{file,[46,46,47,101,116,101,115,116,47|...]},{line,22}]}]})
```

报告显示P进程（父进程)在第23步的时候崩溃了，错误原因时badmatch，z.erl的22行，即
```
ready = read(),
```

此处read()返回的不是ready而是intiallizing，也就是handle_info(timeout...)并没有执行。
上面的１～２３步显示了gen_server的创建过程.以及消息传递过程。十分详细。不过需要挖掘下gen_server的源码才能看懂。

## gen_server的创建过程
下面分析下gen_server的创建过程。假设我们创建一个{local, test}的进程，并传入Args作为初始化参数，Opts为进程的设置参数.

### gen_server:
```
start_link({local, test}, test, Args, Opts) ->
   gen:start(gen_server,  link, {local, test}, test, Args, Opts).
```

### gen:
```
start(gen_server, link, {local, test}, test, Args, Opts) ->
     case where({local, test}) of %检测名字是否注册
       undefined ->
          do_spawn(gen_server, link, {local, test}, test, Args, Opts);
        Pid ->
           {error, {already_started, Pid}}
      end.
```
```
do_spawn(gen_server, link, {local, test}, test, Args, Opts) ->
        Time = somefun(Opts) % 提取出Opt中的timeout参数, 
        proc_lib:start_link(gen, init_it, 
        [gen_server, self(), self(), {local, test}, test, Args, Opts], Time, spawn_opts(Opts)).
```

> 此处spawn_opts的作用时查找Opts中有没有spawn_opt这个选项，否则为[].

### proc_lib:
上面self()一般情况下为supervisor的Pid。
```
start_link(gen, init_it, [gen_server, Sup, Sup, {local, test}, test, Args, Opts], Timeout, Opts|[]) ->
  Pid = proc_lib:spawn_opt(gen, init_it, [gen_server, SelfPid, SelfPid, {local, test}, test, Args, Opts|[]], ensure_link(Opts|[])), %ensure_link尝试将link加入Opts
  sync_wait(Pid, Timeout).
```

```
spawn_opt(gen, init_it, [gen_server, selfPid, SelfPid, {local, test}, test, Args, Opts], [link]|[link, Opts]) ->
   Parent = ......%获取当前进程registername为父进程
   Ancestors = ....% 获取进程字典中的$ancestors的值
   check_for_monitor([link]|[link,Opts])....% 检测monitor这个参数在不在其中，在的话直接抛出错误。 
   erlang:spawn_opt(proc_lib, init_p, [Parent, Ancestors, gen, init_it, [gen_server, SelfPid....Opts],[link]|[link, Opts]).
```

```
sync_wait(Pid, TimeOut) ->
  receive 
  {ack, Pid, Return} ->
    Return;
  {'EXIT', Pid, Reason} ->
    {error, Reason}
  after TimeOut ->
    unlink(Pid),
    exit(Pid, kill),
    flush(Pid),
    {error, timeout}
   end.
```

erlang:spawn_opt与spawn类似，只不过支持一些option。所以调用的还是proc_lib的init_p。
```
init_p(Parent, Ancestors, gen, init_it, [gen_server, Sup, Sup, {local, test}, test, Args, Opts|[]], [link|[link, Opts]) ->
  put('ancestors', [Parent|Ancestors]),
  put('initial_call', trans_init(gen, init_it, [gen_server, Sup.....])),
  init_p_do_apply(gen, init_it, [gen_server, Sup, Sup....]).
```

其中trans_init的结果返回为{Module, init, 1} 即 {test, init, 1}.
```
init_p_do_apply(gen, init_it, [gen_server, Sup, Sup, {local, test}, test, Args, Opts|[]]) ->
    try
	apply(gen, init_it, [gen_server, Sup... Opts]) 
    catch
	Class:Reason ->
	    exit_p(Class, Reason)
    end.
```

exit_p里面格式化一些错误信息，并用error_logger输出

### gen
```
init_it(gen_server, Sup, Sup, {local, test}, test, Args, Opts|[]) ->
    case name_register({local, test}) of
	true ->
	    init_it2(gen_server, Sup, Sup, {local, test}, test, Args, Opts|[]);
	{false, Pid} ->
	    proc_lib:init_ack(Sup, {error, {already_started, Pid}})
    end.
```

```
name_register({local, Name} = LN) ->
    try register(Name, self()) of
	true -> true
    catch
	error:_ ->
	    {false, where(LN)}
    end;
```

```
init_it2(gen_server, Sup, Sup, {local, test}, test, Args, Opts|[]) ->
   gen_server:init_it(Sup, Sup, {local, test}, test, Args, Opts|[]).
```

### gen_server:
```
init_it(Sup, Sup, {local, test}, test, Args, Opts|[]) ->
    test = name({local, test}),
    Debug = debug_options(Name, Opts|[]),
    case catch test:init(Args) of
	{ok, State} ->
	    proc_lib:init_ack(Sup, {ok, self()}), 	    
	    loop(Sup, test, State, test, infinity, Debug);
	{ok, State, Timeout} ->
	    proc_lib:init_ack(Sup, {ok, self()}), 	    
	    loop(Sup, test, State, test, Timeout, Debug);
	{stop, Reason} ->
	    unregister_name(test),
	    proc_lib:init_ack(Sup, {error, Reason}),
	    exit(Reason);
	ignore ->
	    unregister_name(test),
	    proc_lib:init_ack(Sup, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    unregister_name(test),
	    proc_lib:init_ack(Sup, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Sup, {error, Error}),
	    exit(Error)
    end.
```

### proc_lib
```
init_ack(Sup, Pid|{error, already_exited}) ->
    Parent ! {ack, self(), Pid}|{error, already_exited}},
    ok.
```

```
loop(Sup, test, State, test, Timeout, Debug) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  timeout
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, false).
```
至此gen_server初始化完成。
。
由此得知。gen_server再调用test：init之前，主要做了
1. 判断名字是否被占用，否则报{error, already_exited}
2. 写入ancestors和initial call

Opts中的timeout参数主要用来衡量创建进程的time，超时回结束进程并返回。而{ok， State， Timeout}的timeout参数用来参与loop循环，在进程创建后timeout时间内没有接到消息，就会像自己发送timeout这个消息。 而且这个参数在之后的handle_call, handle_cast,handle_info等回调中，使可以修改timeout的数值。参考源码：
```
handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, Debug) ->
    case catch Mod:handle_call(Msg, From, State) of
	{reply, Reply, NState} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, infinity, Debug1);
	{reply, Reply, NState, Time1} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, Time1, Debug1);
	{noreply, NState} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, Debug1);
	{noreply, NState, Time1} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, Debug1);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Name, Msg, Mod, NState, Debug)),
	    reply(Name, From, Reply, NState, Debug),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Parent, Name, Msg, Mod, State, Debug)
    end;
```

可见handle_call的返回值如果包含Time1，就会修改loop的Timeout参数。

## where is the point?
问题出现在
```
	{ok, State, Timeout} ->
	    proc_lib:init_ack(Sup, {ok, self()}), 	    
	    loop(Sup, test, State, test, Timeout, Debug);
```

Ｐ进程创建Ｐ.1进程，然后进入receive，期待一个{ack, Pid, Return}的回复。在init结束返回{ok, State, Timeout}后，proc_lib:init_ack完成了向P发送{ack, Pid, Return}的任务。然后进入ｌｏｏｐ循环。但这时Ｐ进程接到消息后立刻发送了read.导致timeout并没有发送出去。造成badmatch的结果。

在一种情况，其实在name_register的时候名字和Ｐｉｄ就已经注册好了。但此时甚至没有进入ｉｎｉｔ函数，如果有其他进程朝test名字的ｓｅｒｖｅｒ发送消息，自然会导致ｔｉｍｅｏｕｔ的失效。


