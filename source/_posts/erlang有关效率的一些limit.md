title: erlang有关效率的一些limit
date: 2014-08-05 20:57:32
tags: erlang
categories: erlang
---

 Erlang Efficiency Guide
 
###1. 数据类型的空间消耗
 基本单位: word
 在32位架构中占32位,即4bytes
 在64位架构中占64位,即8bytes

<!--more-->

 | data type |  memory cost |
 |------------|:-------------|
|small integer|1 word :|
|              |On 32-bit architectures: -134217729 < i < 134217728 (28 bits)|
|             |On 64-bit architectures: -576460752303423489 < i < 576460752303423488 (60 bits) |
|big integer | 3..N words |
|Atom |1 word :值得注意的是原子储存在一个原子表中,这个原子表同样耗空间,而且不会被回收 |
|float| On 32-bit architectures: 4 words / On 64-bit architectures: 3 words|
|binary|3..6 + data (can be shared)|
|list|1word + 每个元素分配一个word + 每个元素的大小|
|String|1word + 每个元素分配两个word,同list 只不过每个元素的大小就是一个word|
|Tuple |2words + 每个元素的大小|
|Pid|本地节点一个word,其他节点5 words. 另外 port table node table 都耗空间|
|reference|32位:本地节点5 word ,其他节点7 word 64位:本地4word  其他地方6word|
|Fun|9..13 words + size of environment :fun table also cost momery|
|ets|初始时需要768 words + 为每个元素分配该元素的的大小加6words |
|process|327 words when spawned including a heap of 233 words.|

> **Note** : integer 为什么少了4个bits?我去stackoverflow上问了问,大致是用来判断类型的,这4个bits用来区别list,tuple,pid 等等.而32,64位架构指的是你的编译系统,可以在启动erlang的时候看到
`Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]`
我的erlang环境没有显示,但是用`erlang:system_info(wordsize)`看出,如果为4就是4bytes即32bits如果为8就是64bits

所以能用atom就用atom,别用string.

###2.system limit
#### Process limit
erlang运行时系统默认进程的上限是32768,可以通过erl +P NUMBER 提高到一定数量,NUMBER最高为268435456,即2^28(28 bits).
可以通过`erlang:system_info(process_limit)`查看当前系统的进程数上限
#### atom limit
* 一个原子最多由255 个字符构成
* atom最多为2^20个, 即 1048576个,可以通过erl +t NUMBER 增加或者降低,暂时还不清楚能增加到多少,doc上没说

#### ets limit
最多为1400个,可以通过修改ERL_MAX_ETS_TABLES环境变量提高

#### element in tuple 
元组中的元素最多为2^26 即67108863个,但是由于memory的限制,一般达不到这么多.

#### size of binary 
32位环境中,binary最大不能超过2^29(536870911),64位中不能超过2^61
#### node name
因为是atom 所以不能超过255个character
#### open port
默认最多为1024, 可以通过修改ERL_MAX_PORTS到2^28
### 函数的参数
最多为255
