title: makefile笔记
date: 2015-09-29 19:33:58
tags: makefile
---

Makefile
<!-- toc -->

<!--more-->

## Base

> target (目标) : prerequisites (依赖文件)
     command (命令,生成规则）
     
依赖比目标新，command执行

## Rule

### 规则
* 变量声明
Name = sth
类似宏，$(Name)等同于等号右边的
* `#`为注释，\可以转义
* 命令需要**Tab**键开始
* 文件名一般为makefile，Makefile。也可通过`make -f/--file FileName` 指定
* include <filename> 用filename替换当前内容
  * include不能已Tab开始。
  * 没指定路径
     * 从当前目录
     * `make -I/--include-dir`指定的路径
     * /usr/local/bin, /usr/include
     * -include 加上`-`可以让其忽略错误
* 变量声明通配符不起作用。
  object = *.erl 指的就是"*.erl", 可以使用`$(wildcard *.erl)`, 匹配所有erl文件.
* 搜寻路径
  * VPATH = DIR1:DIR2... (VPATH = /usr/lib:...)
  * vpath FileName DIR1:DIR2 (vpath %.erl ../src:../deps)
* 伪目标 .PHONY:target 伪目标是标签。它并不生成。
* 静态规则： target：目标匹配：依赖匹配 . 通过匹配筛选出target中满足 目标匹配的，然后再进行依赖匹配
```
objects=$(wildcard *.o)
all: $(objects)
$(objects):h?.o : h?.c
...
```

### 命令
* @command 不会输出命令内容 ex:`@echo haha`
* `make -n/--just-print` 只显示，不执行
* `make -s/--silent` 全部禁止显示 
* 当命令依赖前一条命令时应该用分号隔开。 `cd test;pwd` ,如果写到两行里会不起作用
* 同指定路径，`-`可以让命令忽略错误，而不至于因为一个命令的错误而终止。
* 变量导出 export value1 =(:=) value2. MAKEFLAGS是make的参数与SHELL默认传递。
* 定义命令包: `define Name function() endef`

### 变量
* 字符，数字，下划线。可以数字开头，大小写敏感
* `A = $(B), B = somevalue` 变量定义可以用到后面定义的变量。
* 使用`:=`时，不可以使用后面的变量
* `A？= B` 用于当A前面没有定义时赋予它B，否则什么都不做。
* 替换： 
```
foo = a.erl b.erl c.erl 
beam = $(foo:.erl=.beam) 
beam2 = $(foo:%.erl=%.beam)
```

两种方式都将erl替换成beam
* `+=` 追加变量
* 目标变量：target: 变量声明  将变量的赋值作用在某一规则里
ex:
```
all: DIR=./src
all: a.erl
   erl -pa $(DIR)
```

* 模式变量 设置所有匹配的目标的变量
```
%.beam: DIR=./src
```


### 条件判断
* `ifeq (A, B) ... else ... endif`
* `ifneq (A, B) ... else ... endif`
* `ifdef A ... else ... endif`
* `ifndef A ... else .... endif`
* 不要把自动化变量`$@`等放入判断。因为判断实在加载Makefile时就计算出来的。运行时变量不能放入

### 函数
#### string
* `$(function, arguments),` 或者用`{`扩起来
* `$(subst, <from>, <to>, <text>)`  把text中的from替换成to
* `$(patsubst, <pattern>, <replacement>, <text>)`
* `$(strip, <string>)` 去掉开头和结尾的空格
* `$(findstring, <find>, <text>)` 查找text中的find
* `$(filter, <pattern> ,<text>)` 过滤，取出符合pattern的
* `$(filter-out, <pattern> ,<text>)` 与上面相反
* `$(sort, <list>)` 排序并取出重复的词 
* `$(word, <n>, <text>)` 取出text第n个词
* `$(wordlist, <begin>, <end>, <text>)` 取出text中begin到end之间的单词
* `$(words, <text>)` 统计单词个数
* `$(firstword, <text>)`  取出第一个
* `$(lastword, <text>)`
 
#### file
* `$(dir <names>)` 返回name所在目录
* `$(notdir <names>)` 返回非目录部分
* `$(suffix <names>) 后缀函数` `a.erl b.erl -> ".erl .erl"`
* `$(basename <names>)` 前缀函数 `a.cpp -> a`
* `$(addsuffix <suffix>, <names>)`  给name添加后缀  `addsuffix .erl, hello -> hello.erl`
* `$(addprefix <prefix>, <names>)`
#### list
* `$(join <list1>, <list2>)` 连接list1和list2中的单词`join aaa bbb, 11 22 33 -> "aaa11, bbb33, 33"
* `$(foreach <var>, <list>, <text>)` 类似于lists:foreach(fun(H) -> ....end, List), text就是fun, var是H,list是List
* `$(if <condition>, <then>, <else>)` else可省略

* `$(call <expression>, <param1>,<param2>...)`
 param1和param2就是$(1),$(2).expression是对这些变量的操作表达式。
```
reverse = $(2) $(1)
$(call reverse, a,b) -> "b a"
```

call的变量数量不限。

* `$(origin <variable>)` 
* `$(shell cmd)`   例如 `Files = $(shell ls)`

* `$(error msg)` `$(warning msg)`  error会终止， warn不会 

### 隐含规则
[ubuntu社区](http://wiki.ubuntu.org.cn/%E8%B7%9F%E6%88%91%E4%B8%80%E8%B5%B7%E5%86%99Makefile:%E9%9A%90%E5%90%AB%E8%A7%84%E5%88%99)
[GUN make](https://www.gnu.org/software/make/manual/html_node/Catalogue-of-Rules.html#Catalogue-of-Rules)


## SomeMore

* 递归调用make的时候一般使用的是`$(MAKE)`代替make。它的的展开其实就是调用make的路径，比如`/usr/bin/make`
它有个特点就是不会传递`-t -q -n`参数。详见
[GUN MAKE variable](http://www.gnu.org/software/make/manual/html_node/MAKE-Variable.html#MAKE-Variable)
* [make options summary](http://www.gnu.org/software/make/manual/html_node/Options-Summary.html#Options-Summary)
