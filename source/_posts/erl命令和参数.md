title: erl命令和参数
date: 2015-07-22 10:11:37
tags: erlang

---


## Startup
先拿mochiweb生成的一个例子说起，这是start-dev.sh脚本中的内容。启动一个mochiweb应用调用的就是这个
```
#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname helloworld_dev \
    -s helloworld \
    -s reloader
```

<!--more-->
通用形式就是
> erl -a b --sth -c d +f g -extra abc

erl后面的都是参数arguments
arguments可以分为**emulator flags**，**flags**，**plain arguments**
    * 任何已+开始的参数会被解释为emulator flags
    * 任何以-开始的参数会被解释为flags，并传入交给了init模块。
    * 在--之后到任何-或者+符号之间的所有flags以及-extra之后所有的flags都是plain arguments，这些参数不会发生解释行为，只不过被储存起来。同样交给init模块。
init:boot(Args)函数接受上面的flags和plain arguments。

>**boot(BootArgs) -> no_return()
Types:
BootArgs = [binary()]**
Starts the Erlang runtime system. This function is called when the emulator is started and coordinates system start-up.
BootArgs are all command line arguments except the emulator flags, that is, flags and plain arguments. See erl(1).
init itself interprets some of the flags, see Command Line Flags below. The remaining flags ("user flags") and plain arguments are passed to the init loop and can be retrieved by callingget_arguments/0 and get_plain_arguments/0, respectively.

flags会被boot函数分为 init flags和user flags。user flags可以通过init:get_arguments()得到，plain arguments通过init:get_plain_arguments()得到。值得注意的是一小部分的“-”后面的flags现在是属于emulator flags。后面提到。

<!-- toc -->

## Init Flags
### --(两个横杠)
任何在--到下一个-或者+之间的都是plain arguments，可以通过init:get_plain_arguments()取到.
### -extra
和--的作用一样，extra后面的flags都是plain arguments
### -code_path_choice Choice::strict|relexed
> When the choice of directories in the code path is strict, the directory that ends up in the code path will be exactly the stated one. This means that if for example the directory \$OTPROOT/lib/mnesia-4.4.7/ebin is explicitly added to the code path, the code server will not load files from $OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin and vice versa.

> This behavior can be controlled via the command line flag -code_path_choice Choice. If the flag is set to relaxed, the code server will instead choose a suitable directory depending on the actual file structure. If there exists a regular application ebin directory,situation it will be chosen. But if it does not exist, the ebin directory in the archive is chosen if it exists. If neither of them exists the original directory will be chosen.
```
"/home/yuyouqi/Erlang/lib/snmp-5.1.1/ebin",
 "/home/yuyouqi/Erlang/lib/sasl-2.4.1/ebin",
 "/home/yuyouqi/Erlang/lib/runtime_tools-1.8.15/ebin",
 "/home/yuyouqi/Erlang/lib/reltool-0.6.6/ebin",
```

```
 "/home/yuyouqi/Erlang/lib/ssh-3.1/ebin",
 "/home/yuyouqi/Erlang/lib/snmp-5.1.1/ebin",
 "/home/yuyouqi/Erlang/lib/sasl-2.4.1.ez/sasl-2.4.1/ebin",
 "/home/yuyouqi/Erlang/lib/runtime_tools-1.8.15/ebin",
 3> code:lib_dir(sasl, ebin).
"/home/yuyouqi/Erlang/lib/sasl-2.4.1.ez/sasl-2.4.1/ebin"
```
### -eval Expr
Expr是个函数，在erl初始化时按顺序执行，如果fail的，则初始化失败。例如
```
erl -eval '{X,Y,Z} = now(), random:seed(X,Y,Z).'
```
> 官方文档Z}旁边多了‘符号，需要去掉

### -run Mod Func Arg1 Arg2....ArgN
调用Mod:Func(["Arg1", "Arg2", ..."ArgN"]),
所有参数都是以string方式传入的。同样按顺序执行，前一个执行完才会执行下一个。但是貌似并不会阻塞shell。
### -s Mod Func Arg1 Arg2 ... ArgN
与run完成同样的功能，但是Arg都是atom的形式传入，值得注意的是数字也会变成atom,
```
erl -s test time 5000 atom
```

就是test:time(['5000', atom]).
一般数字需要atom_to_list, list_to_integer转换。

##User flags

### -ApplicationName Par Val
指定应用的名字，属性名称和值，可以将应用的某个参数设置成Val。
比如
```
 erl -pa ./ebin -s helloworld -helloworld par haha
 1> application:get_all_key(helloworld).
{ok,[{description,"helloworld"},
     {id,[]},
     {vsn,"0.1"},
     {modules,[helloworld,helloworld_app,helloworld_deps,
               helloworld_dtl,helloworld_sup,helloworld_web]},
     {maxP,infinity},
     {maxT,infinity},
     {registered,[]},
     {included_applications,[]},
     {applications,[kernel,stdlib,crypto]},
     {env,[{par,haha},{included_applications,[]}]},
     {mod,{helloworld_app,[]}},
     {start_phases,undefined}]}
2> application:get_all_env(helloworld).
[{par,haha},{included_applications,[]}]
```

启动了一个helloworld应用，并将par参数设置成haha。

### -args_file FileName
直接从FileName读取erl所用的参数。例如在agrsfile的文件中写入如下
> -eval 'io:format("read from file~n")' -extra hello -pa ./ebin

实际结果
```
erl -args_file argsfile
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
1> read from file
1> init:get_arguments().
[{root,["/usr/local/lib/erlang"]},
 {progname,["erl"]},
 {home,["/home/yuyouqi"]}]
2> init:get_plain_arguments().
["hello","-pa","./ebin"]
```

可以看到启动的时候执行了io:format函数，但是-extra这个参数是特殊的，正常情况下-extra到下一个-或者+之间的参数才会被认为是plain arguments，可是如果写在文件中就变成-extra后面所有的参数，都会当成plain arguments.只有这点不同。

### -async_shell_start
名字上来说是异步启动shell

### -boot File
试用File.boot启动erl系统,除非File是个绝对路径，否则在当前文件夹和$ROOT/bin下面找（我的:/usr/local/lib/erlang/bin,  $ROOT可以通过code:root_dir()查看),
默认情况下是-boot start
[script](http://www.erlang.org/doc/man/script.html)
> The command erl -boot Name starts the system with a boot file called Name.boot, which is generated from the Name.script file, using systools:script2boot/1.
The .script file is generated by systools from a .rel file and .app files.

.boot文件是用systools产生的，打开后是乱码.
文件的生成顺序 .rel -> .script -> .boot
[systools](http://erlang.org/doc/man/systools.html)
systools里面提供了生成rel,script, boot等方法。可以在`/usr/local/lib/erlang/releases/R15B`下找到几个官方的rel，script，boot
```
Ubuntu@desktop:/usr/local/lib/erlang/releases/R15B$ ls
start_all_example.rel  start_clean.boot  start_clean.script  start_sasl.rel     start.script
start.boot             start_clean.rel   start_sasl.boot     start_sasl.script
```

打开start_sasl.rel
```
%%注释....
{release, {"OTP  APN 181 01","R15B"}, {erts, "5.9"},
 [{kernel,"2.15"},
  {stdlib,"1.18"},
  {sasl, "2.2"}]}.
```

具体格式在上面的systools链接中有说明
仿照它这个样子写一个test
```
{release, {"test_rel","0.1"}, {erts, "5.9"},
 [{kernel,"2.15"},
  {stdlib,"1.18"},
  {sasl, "2.2"},
  {crypto, "2.1"},
  {helloworld,"0.1"}]}.
```

helloworld使用mochiweb生成的简单框架,用到了crypto，所以讲crypto加入。
```
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
1> systools:make_script("test").       
helloworld: File not found: "helloworld.app"

error
2> systools:make_script("test", [{path, ["ebin"]}]).
ok
```
由于helloworld不在默认的ebin下，所以下加上当前路径中的ebin。否则找不到helloworld.app
如果是使用systools的make_script命令省的script会自动生成.boot。打开test.script
```
%% script generated at {2015,7,1} {12,5,26}
{script,
    {"test_rel","0.1"},
    [{preLoaded,
         [erl_prim_loader,erlang,init,otp_ring0,prim_file,prim_inet,prim_zip,
          zlib]},
     {progress,preloaded},
     {path,["$ROOT/lib/kernel-2.15/ebin","$ROOT/lib/stdlib-1.18/ebin"]},
     {primLoad,[error_handler]},
     {kernel_load_completed},
     {progress,kernel_load_completed},
     {path,["$ROOT/lib/kernel-2.15/ebin"]},
     {primLoad,
         [application,application_controller,application_master,
          application_starter,auth,code,code_server,disk_log,disk_log_1,
          disk_log_server,disk_log_sup,dist_ac,dist_util,erl_boot_server,
          erl_ddll,erl_distribution,erl_epmd,erl_reply,error_logger,
          erts_debug,file,file_io_server,file_server,gen_sctp,gen_tcp,gen_udp,
          global,global_group,global_search,group,heart,hipe_unified_loader,
          inet,inet6_sctp,inet6_tcp,inet6_tcp_dist,inet6_udp,inet_config,
          inet_db,inet_dns,inet_gethost_native,inet_hosts,inet_parse,inet_res,
          inet_sctp,inet_tcp,inet_tcp_dist,inet_udp,kernel,kernel_config,net,
          net_adm,net_kernel,os,packages,pg2,ram_file,rpc,seq_trace,
          standard_error,user,user_drv,user_sup,wrap_log_reader]},
     {path,["$ROOT/lib/stdlib-1.18/ebin"]},
...
...
```

有了.boot文件后就可以使用了
```
yuyouqi@yuyouqi-desktop:~/Erlang/helloworld$ erl -pa ebin deps/*/ebin -boot test
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]


=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.35.0>},
                       {name,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.36.0>},
                       {name,overload},
                       {mfargs,{overload,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.34.0>},
                       {name,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.37.0>},
                       {name,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
         application: sasl
          started_at: nonode@nohost

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
          supervisor: {local,crypto_sup}
             started: [{pid,<0.43.0>},
                       {name,crypto_server},
                       {mfargs,{crypto_server,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
         application: crypto
          started_at: nonode@nohost
** Found 0 name clashes in code paths 

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
          supervisor: {local,helloworld_sup}
             started: [{pid,<0.48.0>},
                       {name,helloworld_web},
                       {mfargs,
                           {helloworld_web,start,
                               [[{ip,{0,0,0,0}},
                                 {port,8080},
                                 {docroot,
                                     "/home/yuyouqi/Erlang/helloworld/priv/www"}]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 1-Jul-2015::12:10:49 ===
         application: helloworld
          started_at: nonode@nohost
Eshell V5.9  (abort with ^G)
1> 
```

### -boot_var Var Dir
指定一个变量Var的路径为Dir，主要用于systools:make_script的{variables, [{"Var", "Prefix"}]},
因为默认的script路径都是在$ROOT/lib下寻找，当在make_script中加入variables选项时，

### -code_path_cache
使用codepath的缓存，这样做的好处是加载模块更快，而且可以在常数时间找到模块，如果没加则需要search这个codepath才能找到对应模块，在模块数量很多时，可以使用。
### -compile Mod1 Mod2 ...
编译Mod1 Mod2 ... 如果失败会返回非零的错误码 默认同时使用了-noinput参数.**不推荐使用，推荐erlc**
### connect_all true|false
是否全联通（N1-N2， N2-N3 -> N1-N2-N3），如果是false那么global模块的注册名字的相关机制无法使用
### -config Config
读取Config文件，名字为AppName.config.
格式
> [{AppName, [{Key1, Value1}...]}, {AppName2, [...]}].

见[config file](http://www.erlang.org/doc/man/config.html).
例如新建helloworld.config
```
[{helloworld, [{key1, val1}, {key2, val2}]}].
```

并在start脚本中加入-config helloworld.config
```
#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname helloworld_dev \
    -s helloworld \
    -s reloader \
    -config helloworld.config
```

如下看到在env中多了写入的config值

```
(helloworld_dev@Ubuntu-desktop)1> application:get_all_key(helloworld).
{ok,[{description,"helloworld"},
     {id,[]},
     {vsn,"0.1"},
     {modules,[helloworld,helloworld_app,helloworld_deps,
               helloworld_dtl,helloworld_sup,helloworld_web]},
     {maxP,infinity},
     {maxT,infinity},
     {registered,[]},
     {included_applications,[]},
     {applications,[kernel,stdlib,crypto]},
     {env,[{included_applications,[]},{key2,val2},{key1,val1}]},
     {mod,{helloworld_app,[]}},
     {start_phases,undefined}]}
```

### -cookie Cookie
过时的没有任何效果的标志，是setcookie的错误拼写，用setcookie代替。
### -setcookie Cookie
和erlang:set_cookie(Node, Cookie)的效果一样。给本地结点设置需要给本地结点起个名字-name/sname
### -detached
以background的形式运行erl，与当前shell分离开，但是需要知道如何停止这个系统
[How to stop](http://stackoverflow.com/questions/15464606/erlang-kill-all-processes-running-in-background).
### -emu_args
打印传给emulator的参数，比如在start-dev.sh脚本中加入它，可以看到
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ./start-dev.sh 
Executing: /usr/local/lib/erlang/erts-5.9/bin/beam.smp /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi -- -pa ebin deps/erlydtl/ebin deps/eunit_formatters/ebin deps/merl/ebin deps/mochiweb/ebin -boot start_sasl -sname helloworld_dev -s helloworld -s reloader

Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]
```

### -env Variable Value
设置OS模块的环境变量Variable为Value.
通过os:getenv/0 getenv/1可以得到
### -heart
开启心跳检测，尽管erlang有各种supervisor监管机制，但是如果emulator down掉了就完了，-heart会开启一个heart进程监控整个emulator，在规定时间内如果没有收到心跳，这个进程就会执行HEART_COMMAND参数，如果这个参数没有设定，那么只会弹出警告，而不会重启。一般HEART_COMMAND就是重启emulator。麻烦的是如果设定了heart，但是没有用-sname，是无法回到这个erlang shell的，也就是无法调用init：stop()来终止heart的监控策略。

在没有开启任何erl shell的情况下调用`ps -ef|grep erl`看到跟erl有关的进程如下
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef |grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3329  2862  0 10:58 pts/2    00:00:00 grep --color=auto erl
```

启动一个erl emulator,然后Ctrl + z放到挂起
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ erl 
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
1> 
[1]+  已停止               erl
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef |grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3333  2862  0 11:01 pts/2    00:00:00 /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi --
yuyouqi   3346  2862  0 11:01 pts/2    00:00:00 grep --color=auto erl
```

可以看到3333这个进程后面的命令是开启一个erl emulator的
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ kill -9 3333
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef | grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3366  2862  0 11:04 pts/2    00:00:00 grep --color=auto erl
[1]+  已杀死               erl
```

可以看到将他kill掉是没问题的
下面启动一个-heart erl shell
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ erl -sname test1 -heart -env HEART_COMMAND "erl -sname test1 -heart"
heart_beat_kill_pid = 3369
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
(test1@yuyouqi-desktop)1> 
[1]+  已停止               erl -sname test1 -heart -env HEART_COMMAND "erl -sname test1 -heart"
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef | grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3369  2862  1 11:05 pts/2    00:00:00 /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi -- -sname test1 -heart
yuyouqi   3376  2219  0 11:05 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/epmd -daemon
yuyouqi   3387  2862  0 11:06 pts/2    00:00:00 grep --color=auto erl

yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ heart: Fri Jul  3 11:06:57 2015: heart-beat time-out.
heart_beat_kill_pid = 3390
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef | grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3376  2219  0 11:05 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/epmd -daemon
yuyouqi   3389  3383  0 11:06 ?        00:00:00 sh -c erl -sname test1 -heart
yuyouqi   3390  3389  0 11:06 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi -- -sname test1 -heart
yuyouqi   3410  2862  0 11:07 pts/2    00:00:00 grep --color=auto erl
```

上面命令是我用-heart启动一个erl shell，可以看到多了3369 和3376这两个进程，把erl shell挂起，一段时间后，由于heart进程没有接收到“心跳”，重启了erl shell,pid变为3390。而且多出了一个3389这个进程，而3390这个erl shell是3389的子进程。可以看到3389就是执行我们的HEART_COMMAND
如果不指定HEART_COMMAND
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ erl -sname test2 -heart
heart_beat_kill_pid = 3685
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
(test2@yuyouqi-desktop)1> 
[1]+  已停止               erl -sname test2 -heart
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef|grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3376  2219  0 11:05 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/epmd -daemon
yuyouqi   3389  3383  0 11:06 ?        00:00:00 sh -c erl -sname test1 -heart
yuyouqi   3390  3389  0 11:06 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi -- -sname test1 -heart
yuyouqi   3685  2862  2 12:15 pts/2    00:00:00 /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi -- -sname test2 -heart
yuyouqi   3703  2862  0 12:15 pts/2    00:00:00 grep --color=auto erl
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ jobs
[1]+  已停止               erl -sname test2 -heart
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ fg
erl -sname test2 -heart

(test2@yuyouqi-desktop)1> 
User switch command
 --> q
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ heart: Fri Jul  3 12:16:10 2015: Erlang has closed.
heart: Fri Jul  3 12:16:10 2015: Would reboot. Terminating.

yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef|grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3376  2219  0 11:05 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/epmd -daemon
yuyouqi   3389  3383  0 11:06 ?        00:00:00 sh -c erl -sname test1 -heart
yuyouqi   3390  3389  0 11:06 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi -- -sname test1 -heart
yuyouqi   3705  2862  0 12:16 pts/2    00:00:00 grep --color=auto erl
```

新建一个test2 shell，不设置HEART_COMMAND，
可以看到如果没有设置HEART_COMMAND 使用ctrl g退出后不会重启，但是test1是会重启的。
而且新建节点后发现-daemon进程并没有增加，我猜测-daemon有可能是heart监控进程。
但是无论怎样kill都是无效的，依然会重启
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ kill -9 3376 3389 3390
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ heart: Fri Jul  3 12:21:15 2015: Executed "erl -sname test1 -heart". Terminating.
heart: Fri Jul  3 12:21:15 2015: Erlang has closed.
heart_beat_kill_pid = 3716

yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef|grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3715  3404  0 12:21 ?        00:00:00 sh -c erl -sname test1 -heart
yuyouqi   3716  3715  3 12:21 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/yuyouqi -- -sname test1 -heart
yuyouqi   3723  2219  0 12:21 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/epmd -daemon
yuyouqi   3734  2862  0 12:21 pts/2    00:00:00 grep --color=auto erl
```

这时就需要进入test1，调用init:stop()来结束
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ erl -sname test2
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
(test2@yuyouqi-desktop)1> 
User switch command
 --> r 'test1@yuyouqi-desktop'
 --> c
Eshell V5.9  (abort with ^G)
(test1@yuyouqi-desktop)1> init:stop().
*** ERROR: Shell process terminated! (^G to start new job) ***
heart: Fri Jul  3 12:23:40 2015: Executed "erl -sname test1 -heart". Terminating.

User switch command
 --> j
   1  {shell,start,[init]}
 --> c 1

(test2@yuyouqi-desktop)1>        
User switch command
 --> q
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ ps -ef|grep erl
yuyouqi   2475  2345  0 10:23 ?        00:00:01 /usr/bin/perl /usr/bin/shutter --min_at_startup
yuyouqi   3723  2219  0 12:21 ?        00:00:00 /usr/local/lib/erlang/erts-5.9/bin/epmd -daemon
yuyouqi   3760  2862  0 12:24 pts/2    00:00:00 grep --color=auto erl
```

这样才能将heart监控的shell关闭(或者使用rpc)
参考[Is there a way to kill the erlang vm when it is running with -heart](http://stackoverflow.com/questions/7217892/is-there-a-way-to-kill-the-erlang-vm-when-it-is-running-with-heart)

### -hidden
已hidden启动的结点在于其他结点连接时，他的连接时隐藏的，不会再nodes()中得到自己的名字。只有与自己同属于一个[global_group](http://www.erlang.org/doc/man/global_group.html#type-group_name)的其他结点才能知道自己。
比如建立一个kernel.config,定义一个global_group
```
[{kernel, [{global_groups, [{test, ['test1@yuyouqi-desktop', 'test2@yuyouqi-desktop']}]}]}].
```

然后启动3个结点test1，test2，test3
```
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ erl -sname test1 -config kernel.config 
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
(test1@yuyouqi-desktop)1> global_group:global_groups().
{test,[]}
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ erl -sname test2 -hidden -config kernel.config 
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
(test2@yuyouqi-desktop)1> global_group:global_groups().
{test,[]}
yuyouqi@yuyouqi-desktop:~/Erlang/mochiweb/helloworld$ erl -sname test3 -config kernel.config 
Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
(test3@yuyouqi-desktop)1> global_group:global_groups().
{no_name,[test]}
```

其中test2已hidden形式启动，test3不在global_group中，可以看到test1和test2返回自己的global_group为test。
用`net_kernel:connect`将它们互联。
```
(test2@yuyouqi-desktop)2> net_kernel:connect('test1@yuyouqi-desktop').
true
(test2@yuyouqi-desktop)3> nodes().
['test1@yuyouqi-desktop']
(test2@yuyouqi-desktop)5> net_kernel:connect('test3@yuyouqi-desktop').
true
(test2@yuyouqi-desktop)6> nodes().
['test1@yuyouqi-desktop']
(test1@yuyouqi-desktop)2> nodes().
['test2@yuyouqi-desktop']
(test3@yuyouqi-desktop)2> nodes().
[]
```

test1和test2是可以互相看到的，而test3的nodes()返回的是空

### -init_debug
启动的时候打印调试参数。可以尝试下`erl -init_debug` 然后对照着`/usr/local/lib/erlang/releases/RXXB/start.script`

### -make
相当于make:all(),见
[make](http://www.erlang.org/doc/man/make.html)
### -man Module
查询某个模块的手册，首先需要去官网下载相应的手册
### -mode interactive|embedded
erlang system载入代码的方式，interactive模式下，系统只载入一部分代码，当使用为载入的代码时，会尝试自动载入这个模块。如果是embeded模式，实在系统初始化时，根据启动的script文件载入所有代码，如果文件上没有提到，不会载入，如果调用为载入模块的函数，会报exception error
### -name/sname NAME
```
erl -sname test1
erl -name test2@192.168.1.10
```

sname 与name之间的结点不能通信，必须命名方式相同才可以。 sname只能用于同一子网内的机器。
### -noinput
erl不读入任何参数
### -noshell
常用，不显示erl shell。一般都是执行一系列的-eval，-s，-ran，然后init:stop().
### -pa/-pz 
添加code路径，没什么可说的同code:addpathsa/addpathsz
### -remsh Node
远程链接一个节点。例如在shell1中启动一个mochiweb例子
```
youthy@youthy:~/code/helloworld$ ./start-dev.sh 
...=PROGRESS REPORT==== 17-Jul-2015::12:20:43 ===
          supervisor: {local,kernel_safe_sup}
             started: [{pid,<0.74.0>},
                       {id,timer_server},
                       {mfargs,{timer,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,1000},
                       {child_type,worker}]

(helloworld_dev@youthy)1> 
```

然后在shell2中链接这个节点。
```
youthy@youthy:~$ erl -sname test2 -remsh helloworld_dev@youthy
Erlang R15B (erts-5.9) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V7.0  (abort with ^G)
(helloworld_dev@youthy)1> helloworld_web:
loop/2         module_info/0  module_info/1  start/1        stop/0         

(helloworld_dev@youthy)1> helloworld_web:module_info().
[{module,helloworld_web},
 {exports,[{start,1},
           {stop,0},
           {loop,2},
           {module_info,0},
           {module_info,1}]},
 {attributes,[{vsn,[243820355007545606700617893262330487884]},
              {author,"Mochi Media <dev@mochimedia.com>"}]},
 {compile,[{options,[{outdir,"ebin"},
                     debug_info,debug_info,
                     {i,"include"}]},
           {version,"6.0"},
           {time,{2015,7,17,4,17,8}},
           {source,"/home/youthy/code/helloworld/src/helloworld_web.erl"}]},
 {native,false},
 {md5,<<183,110,23,159,102,166,62,91,144,171,107,229,135,
        128,228,76>>}]
(helloworld_dev@youthy)2> 
User switch command
 --> q
youthy@youthy:~$ 
```

值得注意的是1 必须给节点一个名字且命名方式需要一致。2退出时，不要用q()之类的，否则退出的时对面的节点。需要用ctrl+G然后q的方式。

### -rsh Program
一般用来启用slave节点时用到。下面例子一开始我们进程中没有erl shell先启动一个master节点。然后在master节点里调用slave:start_link(Host, Name, Args)的方式。启动一个slave节点。这时我们看到进程里面有了两个erl shell。其中一个是已-noshell, noinput等方式启动的。这个就是slave启动的奴隶节点。我们可以用rpc调用一个功能，然后结果会返回给master节点。
因为我只有一台机器，如果不用"-rsh ssh"也是可以成功的。但是如果真是在其他机器上还是要加上ssh。两台节点环境，erl版本需一致。master节点退出后，不管时start_link启动还是start启动的slave节点都会退出。
```
youthy@youthy:~$ ps -ef | grep erl
youthy   13884  2086  0 14:47 ?        00:00:00 /home/youthy/erls/R15B/erts-5.9/bin/epmd -daemon
youthy   23280 13177  0 15:19 pts/0    00:00:00 grep --color=auto erl
youthy@youthy:~$ erl -sname master
Erlang R15B (erts-5.9) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
(master@youthy)1> 
[1]+  已停止               erl -sname master
youthy@youthy:~$ ps -ef | grep erl
youthy   13884  2086  0 14:47 ?        00:00:00 /home/youthy/erls/R15B/erts-5.9/bin/epmd -daemon
youthy   23299 13177  1 15:19 pts/0    00:00:00 /home/youthy/erls/R15B/erts-5.9/bin/beam.smp -- -root /home/youthy/erls/R15B -progname erl -- -home /home/youthy -- -sname master
youthy   23336 13177  0 15:19 pts/0    00:00:00 grep --color=auto erl
youthy@youthy:~$ fg
erl -sname master

(master@youthy)1> slave:start_link('youthy', 'slave', "-rsh ssh").
{ok,slave@youthy}
(master@youthy)2> 
[1]+  已停止               erl -sname master
youthy@youthy:~$ ps -ef | grep erl
youthy   13884  2086  0 14:47 ?        00:00:00 /home/youthy/erls/R15B/erts-5.9/bin/epmd -daemon
youthy   23299 13177  0 15:19 pts/0    00:00:00 /home/youthy/erls/R15B/erts-5.9/bin/beam.smp -- -root /home/youthy/erls/R15B -progname erl -- -home /home/youthy -- -sname master
youthy   23546  2086  0 15:20 ?        00:00:00 /home/youthy/erls/R15B/erts-5.9/bin/beam.smp -- -root /home/youthy/erls/R15B -progname erl -- -home /home/youthy -- -noshell -noinput -noshell -noinput -master master@youthy -sname slave@youthy -s slave slave_start master@youthy slave_waiter_0 -rsh ssh
youthy   23587 13177  0 15:20 pts/0    00:00:00 grep --color=auto erl
youthy@youthy:~$ fg
erl -sname master

(master@youthy)2> rpc:call('slave@youthy', lists, sum, [[2,3,4]]).
9
(master@youthy)3> 
```

### -set_cookie Cookie
等同于erlang:set_cookie/2

### -shutdown_time Time
Time是ms毫秒。设置关闭erl节点的时间，如果时间到了还没有结束完，就强制结束掉还存在的进程。
```
youthy@youthy:~$ erl -shutdown_time 1000
Erlang R15B (erts-5.9) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
1> q().
ok
2> {init,shutdown_timeout}
youthy@youthy:~$ 
youthy@youthy:~$ erl -shutdown_time 4000
Erlang R15B (erts-5.9) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9  (abort with ^G)
1> q().
ok
2> youthy@youthy:~$ 
```

如上，1秒的时间看来时不够用的。
### -smp enable/auto/disable
是否启用smp，默认时auto。auto自动检测。如果符合会自动开启smp。
[关于smp](http://www.cnblogs.com/me-sa/archive/2012/02/01/erlang0035.html).
### -version
打印version信息。
