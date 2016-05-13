title: vimscript笔记
date: 2016-05-06 15:33:21
tags: vim
toc: true
---


> Remeber `:help` always be with you  -- albert einstein
记住`:help`　与你同在

<!-- more -->
<!-- toc -->

### First of all
首先`:help echo/function/....`　help可以查看一个关键词的帮助.首先要习惯使用它

### echo echom  打印信息帮助调试
```
:echo "test"
:echo "test2"
:messages
```

test2在message中，而test不在。
echom 主要用来调试。

### 注释
```
" 这是注释
```

### (布尔)选项

`:set` 用于设置内置的选项。有的是布尔选项，有的不是 see `:help set`
```
:set number
:set nonumber
:set number!
:set number?
:set number numberwidth=4
:set omnifunc=XXX#complete
```

number是一个布尔选项。任何布尔选项都可以通过`set` ,`set no(Bool)`, 设置开启关闭`set Bool!`切换开关状态.
`set {Option}?` 用来查看当前值. 
有些选项可以赋值.多选项用空格隔开一次赋值
number是用来显示行号的布尔值
omnifunc 是补全函数
在脚本中如果要引用某个选项使用`&`, 比如
```
if &filetype == "vim"
```

判断当前文件类型是否是vimscript

### map 映射!映射!
`:help map-commands`
```
:map - x  //  任何模式下将'-'映射为'x'
:map <space> x
:map <c-d> x
:nmap - x
:vmap - x
:imap - x
:noremap - x
:noremap \ -
```

map通用于所有模式下。nmap,vmap, imap分别映射在不同的模式下。
noremap不存在递归风险。任何时候都应该使用这种方式。**对，任何时候**

### leader按键
`:help mapleader`
很多插件中存在各种映射都用的<leader>这个按键
```
:let mapleader="-"
:nnoremap <leader>d dd
let maplocalleader = "\\"
:noremap <buffer> <localleader>d dd
```

leader 默认为`\`, 如果修改可以通过`echo mapleader`查看
localleader 只针对某个buffer
> 除了map 还有各种其他地方指定`<buffer>`的设置。比如autocmd,加上buffer比较安全。

同样有的选项也支持`:setlocal number`

### iabbrev
```
:iabbrev waht what
```

自动替换。　可以将ｗａｈｔ　替换为ｗｈａｔ
iabbrev是结束insert mode时发生的。 如果what前后有别的则不会发生替换.这点与`inoremap waht what`不同
同样可以接受`:iabbrev <buffer> XXXX`　只对当前buffer生效

### autocmd 自动命令
`:help autocmd`
`:help Event`
```
:autocmd BufNewFild,BufRead *.html :normal gg=G
         事件                模式   执行命令
```

`autocmd Event File Command`  对File文件接收到event事件后　触发command

Event后面的模式每个pattern不一样。
比如 `:autocmd FileType vim call SomeFun()`
**这里有个危险的地方**
单纯的`autocmd` 并不会覆盖之前的命令。如果同一个`autocmd`加载多次他们会同时存在也就是可能对于同一个事件执行多次`SomeFun()`.
这里vim有一种自动命令组的方法
```
:augroup testgroup
:    autocmd BufWrite * :echom "Foo"
:    autocmd BufWrite * :echom "Bar"
:augroup END
```

上面将"Foo", "Bar"两个事件归到testgroup组中。这时testgroup相当于一个`autocmd`　同样多次执行testgroup也会造成上面的情况。同样，如果之后在执行
```
:augroup testgroup
:    autocmd BufWrite * :echom "Baz"
:augroup END
```

"Foo", "Bar"并没有清除.而是将"Baz"合并到了testgroup中.这样没有解决多次读取事件造成重复调用的问题。
解决的办法是在组开始的地方调用 `autocmd!`来清除这个组的事件
```
:augroup testgroup
:    autocmd!
:    autocmd BufWrite * :echom "Cats"
:    autocmd XXXXX....
:augroup END
```

这样解决了多次重复调用的问题

### operator_pending

`:onoremap p ic`

w, p, t 各种定位词　ｏｎｏｒｅｍａｐ可以修改定位词映射

### :normal
`:normal gg`
`:normal` 后面会跟一个字符串。相当于在ｎｏｒｍａｌ模式下按下这些按键

当然永远不要用normal 而是用`normal!` 后者无视映射。因为你永远不知道用户把按键映射成了什么

### :execute
`:execute ":normal!  gg"

execute 后面跟脚本。

为什么不直接用normal!,而是用execute呢
因为normal不能识别特殊字符。比如<cr>回车符等所以一般要包个execute命令。
比如`:normal! a\<cr>` 我们设想先按下a进入insert模式然后输出回车符换行.但是实际上这个命令进入插入模式后，直
接将`\<cr>`输入到了buffer中.我们可以用`execute "normal! a\<cr>"完成这个动作

### set statusline
```
:set statusline=%f\ -\ %y
:set statusline+=%=
:set statusline+=%L
```

修改状态条显示内容。类似printf %f 是文件名%y是文件类型 %=表示后面的右对齐%L显示总行数。

### let  变量
```
:let foo="bar"
:echo foo  //显示ｂａｒ
:set number
:echo &number //1
:let number = 100
:echo number //100
:let &number=&number-1
:echo &number //0　number是是否显示行号的布尔值
```

let 用来定义一个变量,这个变量可以与某个选项同名(set), 只不过选项的引用需要前面加`&`
set 只能给选项赋值常量值
而let 可以用变量给变量赋值,如上面的`&number`
`:let &l:number = 100`可以给本地变量赋值而不影响其他buffer

### 寄存器
`:register` 查看所有寄存器内容
`"ap` a是寄存器名字. 表示将a中的内容粘贴到此处
`echo @a` 打印a中的内容
`let @a="test"` 修改a中内容
`"`寄存器是为指定寄存器所有为指定寄存器的复制操作,内容都会写入这里

### 管道
`:echo "haha" | echo "heihei`


### if
```
:if "20haha"
:   echo "true"
:endif

:if "haha20"
:   echo "true"
:else
:   echo "false"
:endif
```

第一个会显示ｔｒｕｅ　第二个是ｆａｌｓｅ　
因为字符串在强制转换时第一个是数字会转化为相应的数字。否则是０

#### 比较
不能完全信任`==`
因为vim的大小写敏感时用户设置的
```
:set ignorecase
:if "foo" == "FOO"
```

这个语句真假取决于ignorecase的设置。
替代品
`==?` 大小写不敏感比较
`==#` 大小写敏感比较

### 函数
```
:function Test()
:   XXXXX
:endfunction
:function s:testb()
:   echom "script test"
:endfunction
:call Test()
```

**没有限制作用域的函数必须以大写字母开头**
脚本内的私有函数以`s:`开头
函数没有显示指定`return XX`则默认返回0


#### 参数
```
:function EchoTest(name)
:   echo a:name
:endfunction
```

函数的参数总要加a这个作用域否则会提示找不到

#### 可变参数
```
:function Test(...)  // 假如 call Test("a", "b", "c")
:   echo a:0         // 3 参数个数
:   echo a:1         // "a"
:   echo a:000       // ['a', 'b', 'c'] 000指参数列表.不能用echom输出只能用echo
:endfunction
```

`:call Test("a", "b", "c")`
a:0 指的是参数数量。 a:1 是 "a".以此类推

#### 命名空间
> |buffer-variable|    b:	  Local to the current buffer.
|window-variable|    w:	  Local to the current window.
|tabpage-variable|   t:	  Local to the current tab page.
|global-variable|    g:	  Global.
|local-variable|     l:	  Local to a function.
|script-variable|    s:	  Local to a |:source|'ed Vim script.
|function-argument|  a:	  Function argument (only inside a function).
|vim-variable|	     v:	  Global, predefined by Vim.

命名空间可以当做dict使用
``` 
for k in keys(b:)
    unlet b:[k]
endfor
```

可以混合可变参数和固定参数
`:function Test(a, ...)`

不能对函数的参数改变赋值
```
:function Test(foo)
:   let a:foo="change" //这样做会报错
:   let tmp=a:foo      // 这样才可以
:   let tmp="change"
:endfunction
```

### 字符串
#### 连接符
```
:echo "haha"."heihei" 
```

上面输出"hahaheihei"

#### 字符串函数
`:help function-list` 查看所有函数分类列表

`strlen("string")`
`len("string")`
`echo split("one two")`
`echo split("one,two",",")`
`echo join(["one","two"], ",")`
`echo tolower("BIG")`
`echo toupper("low")`

### 列表
1. 有序，异质
2. 索引从0开始 
```
:echo [1,2,3][0] // 1
:echo [1,2,3][-1] //3
:echo "abc"[1] // b
:echo "abc"[-1] // 无效。
```

3. 切割
```
:echo [1,2,3][:1] // [1,2]
:echo [1,2,3][1:] // [2,3]
:echo [1,2,3][:1000] // [1,2,3] 可以越界
:echo [1,2,3,4][-2:-1] //[3,4]
:echo [1,2,3,4][-2:1] // []
:echo "abc"[-2:-1] // bc。上面的负数索引无效。切割却有效。。。
```

4.连接
```
:echo ['a','b'] + ['c'] // ['a','b','c']
:echo [1,2] + [3]//[1,2,3]
:echo [a,b] + [c] // 错误。
:echo "ab"."c" //"abc"
```

5. 列表函数
```
:echo add([1,2],3) // [1,2,3]
:echo get([1,2,3], 0, 'default') // 1
:echo get([1,2,3], 4, 'default') // default
:echo index([1,2,3], 1) // 0
:echo index([1,2,3], 4) // -1 不存在返回-1
:echo join([1,2], "-") // 1-2
:echo reverse([1,2]) //[2,1]
```

更多阅读`:help List` `:help functions`

### 循环
#### for
```
:for i in [1,2,3,4]
:   let c += i
:endfor
```

```
:let c=1
:while c < 3
: let t += c
: let c += 1
:endwhile
```

### dict
`{'a':1, 2:"two",}` 字典的key总是会自动转为字符串.  应该总是结尾留一个`,` (个人很讨厌这种语法)
索引可以用.来索引
```
:echo {'a':1, 2:"two",}['a'] // 1
:echo {'a':1, 2:"two",}[2] // two
:echo {'a':1, 2:"two",}.a // 1
```

增加键值
```
:let dic={'a':1,}
:let dic.b=2
:echo dic // {'a':1, 'b':2}
```

```
:let tmp=remove(dic, 'a') //移除dic中a，但是把a的值赋值给tmp
:unlet dic.b //移除b。但是不返回值
```

```
:echo has_key({'a':1}, 'a') // 1
:echo has_key({'a':1}, 'b') //0
:echo items({'a':1, 'b':2}) // [['a',1],['b',2]] items返回是无序的。因为dict本身是无序的
:echo keys({'a':1, 'b':2}) // ['a', 'b']
:echo values({'a':1, 'b':2}) //[1,2]
```

list中的get函数同样可以用于dict
### 函数式编程
1. 在函数内部赋值传入的参数。已达到不影响外部变量。
```
function! Reversed(l)
    let new_list = deepcopy(a:l)
    call reverse(new_list)
    return new_list
endfunction
```

#### deepcopy 与copy
 区别如下
```
let a=[1,2]
let b=[a,3,4] // [[1,2],3,4]
let c=copy(b) // [[1,2],3,4]
let d=deepcopy(b) //[[1,2],3,4]
let b[0][0]=5
let b[1]=6
echo a  //[5,2]
echo b  //[[5,2],6,4]
echo c  //[[5,2],3,4]
echo d  //[[1,2],3,4]
```

2. 将函数赋值给变量。变量名字首字母大写
```
:let Myfunc = function("add")
:echo Myfunc([1,2],3) // [1,2,3]
:let funclist=[function("add"), function("reverse")]
:echo funclist[0]([1,2],3) // [1,2,3]
```

### 路径
```
:echo expand('%') // filename  相对路径
:echo expand('%:p') // /home/.../filename 绝对路径
:echo fnamemodify('test.erl', ':p') // /home/youthy/.../test.erl 无论文件test是否存在
:echo globpath('.', '*.erl') // 显示当前目录下所有erl文件的路径
:echo split(globpath('.','*.erl'), '\n') //将上述结果分割成列表
```

`:set runtimepath=/XX/XXX` 修改运行时路径。

### Regex 正则表达式
for starters `:help user_27`

所有的search 受大小写设置的影响
```
:set ignorecase // 忽略
:set noignorecase // 不忽略大小写
:set ingorecase smartcase //智能模式。 小写时match所有。有一个大写字母时精确匹配。
```

`\c`, `\C` 无视ignorecase设置。用于自己某个特殊的匹配。前者 表示ignorecase 后者是noignorecase
>	pattern			matches	~
	\Cword			word
	\CWord			Word
	\cword			word, Word, WORD, WoRd, etc.
	\cWord			word, Word, WORD, WoRd, etc.

`*` 0至多个 
`\+` 1至多个
`\=` 0至1个
`'\{n, m}'` 重复n至m次
`'\\|'` 或者
```
'ab*'   // a, ab, abb, abbb...
'c\(ab\)*' // c, cab, cabab, cababab...  需用括号括起，但是需要转义
'c\(ab\)\{2, 3}' // cabab, cababab
'ab\+' // ab, abb, abbb...
'ab\=' // a, ab
```

>	item	matches			equivalent ~
	\d	digit			[0-9]
	\D	non-digit		[^0-9]
	\x	hex digit		[0-9a-fA-F]
	\X	non-hex digit		[^0-9a-fA-F]
	\s	white space		[ 	]     (<Tab> and <Space>)
	\S	non-white characters	[^ 	]     (not <Tab> and <Space>)
	\l	lowercase alpha		[a-z]
	\L	non-lowercase alpha	[^a-z]
	\u	uppercase alpha		[A-Z]
	\U	non-uppercase alpha	[^A-Z]
	\h [A-Za-z_]
	\w [A-Za-z0-9_]

以上不能用于`[]`里面

注意　`'`, `"`的影响。`"`会转义`\`, 导致`'\w\+'`这种变成`"\\w\\+"`



[笨方法学vimscript](http://learnvimscriptthehardway.onefloweroneworld.com/)





    




