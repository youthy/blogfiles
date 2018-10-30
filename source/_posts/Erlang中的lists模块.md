title: Erlang中的lists模块
date: 2014-07-11 21:13:31
tags: erlang 
categories: erlang
---

> 个人感觉erlang里面lists模块用的地方非常多,因为erlang中没有for循环,很多函数功能多是靠lists的map,filter或者列表解析完成的,所以lists里面的函数非常重要

<!--more-->
<!-- toc -->
### all/2
> all(Func, List) -> boolean()

如果全员执行Func都返回true,则这个表达式返回true,否则返回false.


```erlang
2> lists:all(fun(X) -> is_integer(X) end, [1,2,3,4]).
true
3> lists:all(fun(X) -> is_integer(X) end, [1,2,"haha",4]).
false
```

### any/2
> any(Pred, List) -> boolean()

与all大致相同,如果有任意一个List中的元素返回true,则any返回true.

### append/1
> append(ListOfLists) -> List1

用于合并子列表,列表中元素最起码要有一层深度,而且append只能去掉一层的深度,如果子元素没有深度,就会抛错
```erlang
5> lists:append([[1,2,3],[2,3],[1],[2]]).
[1,2,3,2,3,1,2]
7> lists:append([[1,2,3],[2,3],[1],[[1,[3,2]],2]]).
[1,2,3,2,3,1,[1,[3,2]],2]
8> lists:append([1,[2,3],[3]]).
** exception error: bad argument
     in operator  ++/2
        called as 1 ++ [2,3,3]
     in call from lists:append/1 (lists.erl, line 74)
```

因为在append的源码是:
> 73 append([E]) -> E;
  74 append([H|T]) -> H ++ append(T);
  75 append([]) -> []. 

所以上面说 `++`附近出错,因为1不是列表.


### append/2
> append(List1, List2) -> List3

源码
>  append(L1, L2) -> L1 ++ L2.

用于合并两个列表,其实append实质就是++,所以需要注意效率,因为++是将左边的列表都一遍,因此在递归中要始终保持左边的列表要是最短的那个.
```erlang
9> lists:append("haha","heihei").
"hahaheihei"
```

### concat/1
> concat([Things]) -> string()

Things可以使integer,float,atom,string, concat将他们转换成一个字符串,如果Things中含有list,可能会整体装换成list.
例如
```
10> lists:concat(["haha",[1],3,3.14,atom]).
[104,97,104,97,1,51,51,46,49,52,48,48,48,48,48,48,48,48,48,
 48,48,48,48,49,50,52,51,52,101|...]
11> lists:concat(["haha",3,3.14,atom]).    
"haha33.14000000000000012434e+00atom"
```
源码:
```erlang
concat(List) ->
    flatmap(fun thing_to_list/1, List).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.	%Assumed to be a string
```

### delete/2
> delete(Elem, List) -> List1

删除List中第一个等于Elem的元素,返回一个新的list
```
12> lists:delete(1,[1,2,1,3,4]).
[2,1,3,4]
```

### dropwhile/2
> dropwhile(Func, List) -> List1
 
从左边开始去掉List中Func返回值为true的元素,遇到第一个返回false的元素时终止,并返回剩下列表.
例子:
```
16> lists:dropwhile(fun(X) -> X rem 2 =:= 0  end, [2,4,3,5,6]).  
[3,5,6]
```
源码实现:
```erlang
dropwhile(Pred, [Hd|Tail]=Rest) ->
    case Pred(Hd) of
	true -> dropwhile(Pred, Tail);
	false -> Rest
    end;
dropwhile(Pred, []) when is_function(Pred, 1) -> [].
```

### duplicate/2
> duplicate(N, Elem) -> List.

生成N个Elem的列表.N不能使负数.原因可以看源码
```erlang
-spec duplicate(N, Elem) -> List when
      N :: non_neg_integer(),
      Elem :: T,
      List :: [T],
      T :: term().

duplicate(N, X) when is_integer(N), N >= 0 -> duplicate(N, X, []).

duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).
```
原理就是用列表构造,没添加一个H,N就-1,终止条件就是直到N=0,如果是负数则无法终止,所以不能为负
例子:
```
17> lists:duplicate(3,"haha").
["haha","haha","haha"]
```

### filter/2
> filter(Func, List) -> List1

过滤器,返回列表,元素是List中所有Func返回值为true的元素.这个在工程中经常用.
源码
```erlang
-spec filter(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

filter(Pred, List) when is_function(Pred, 1) ->
    [ E || E <- List, Pred(E) ].
```
可以看到,filter内部其实就是用列表解析实现的.
例子:
```
19> lists:filter(fun(X) -> X rem 2 =:= 0 end, [2,4,3,5,6]).
[2,4,6]
```

### flatlength/1
> flatlength(DeepList) -> integer

该函数返回一个深度列表的长度,如果是深度为1的列表可以直接用length(List)返回长度,
官方doc这么说的
> Equivalent to length(flatten(DeepList)), but more efficient.

等同于深度列表扁平化后在length,但是这个做更搞笑,原因很简单,这个函数只不过计算长度,并没有将列表扁平化
源码
```erlang
-spec flatlength(DeepList) -> non_neg_integer() when
      DeepList :: [term() | DeepList].

flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T], L) when is_list(H) ->
    flatlength(H, flatlength(T, L));
flatlength([_|T], L) ->
    flatlength(T, L + 1);
flatlength([], L) -> L.
```
例子:
```erlang
> lists:flatlength([1,[2,[3],4,[5],6],[7,8]]).
8
```

### flatten/1
> flatten(Deeplist) -> List

上面提到的flatten函数,用于扁平化一个深度列表,尽量少用,代价很高,甚至比++操作代价还高
源码
```erlang
-spec flatten(DeepList) -> List when
      DeepList :: [term() | DeepList],
      List :: [term()].

flatten(List) when is_list(List) ->
    do_flatten(List, []).

-spec flatten(DeepList, Tail) -> List when
      DeepList :: [term() | DeepList],
      Tail :: [term()],
      List :: [term()].

flatten(List, Tail) when is_list(List), is_list(Tail) ->
    do_flatten(List, Tail).

do_flatten([H|T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H|T], Tail) ->
    [H|do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail.
```

在递归中为了伪递归,如果第一个元素是list就递归调用do_flatten,如果不是就用构造器放在新列表的头,剩下的部分继续调用do_flatten.
**关于flatten的效率问题**
在官方doc的efficiency guide中有专门一块Deep and flat lists,
原文这么说
> lists:flatten/1 builds an entirely new list. Therefore, it is expensive, and even more expensive than the ++ (which copies its left argument, but not its right argument).
In the following situations, you can easily avoid calling lists:flatten/1:
1. When sending data to a port. Ports understand deep lists so there is no reason to flatten the list before sending it to the port.
2. When calling BIFs that accept deep lists, such as list_to_binary/1 or iolist_to_binary/1.
3. When you know that your list is only one level deep, you can can use lists:append/1.

因为flatten是完全重建一个列表,甚至比++(复制左边,而右边保留)更为低效,但是有3种情况可以不适用flatten
1. 往端口发送数据时,因为端口懂得深度列表
2. 诸如`list_to_binary`和`iolist_to_binary`的BIF,不需要扁平化
```erlang
> list_to_binary([1,[2,[3,4]],5]).
<<1,2,3,4,5>>
```
3. 如果我们知道列表深度只有一层,要用append代替flatten

### flatten/2
> flatten(DeepList, Tail) -> List.

就是上面flatten/1调用的子函数,flatten(DeepList) 就是flatten(DeepList, []).
函数在DeepList扁平化后append了一下Tail.
```erlang
27> lists:flatten([1,[3,[4,5],2]],[1,[2,[3]]]).
[1,3,4,5,2,1,[2,[3]]]
```

### flatmap/2
> flatmap(Func, List1) -> List2

官方的说法是
> That is, flatmap behaves as if it had been defined as follows:
flatmap(Fun, List1) ->
    append(map(Fun, List1)).

就是List1中每个元素执行Fun,一般Fun返回一个列表,最后将这些列表append一下.
例子:
```erlang
26> lists:flatmap(fun(X) -> [X, X+1] end, [1,3,5,7]).
[1,2,3,4,5,6,7,8]
```
源码:
```erlang
-spec flatmap(Fun, List1) -> List2 when
      Fun :: fun((A) -> [B]),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

flatmap(F, [Hd|Tail]) ->
    F(Hd) ++ flatmap(F, Tail);
flatmap(F, []) when is_function(F, 1) -> [].
```

### foldl/3
> foldl(Fun, Acc0, List) -> Acc1

这个函数用的非常多,Fun是一个可接受2个参数的函数,例如fun(X, Sum) -> X + Sum end.那么List中的第一个元素带入X,然后Acc0带入Sum,执行得结果Acc1,然后将List中第二个元素带入X,将Acc1带入Sum,以此类推,每回fun的第二个参数是上把的结果,第一个元素是List中的元素.下面例子是一个累加函数
例子:
```erlang
28> lists:foldl(fun(X, Sum) -> X + Sum end, 0, [2,4,6,8,10]).
30
```
源码:
```erlang
-spec foldl(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) -> Accu.
```

上面可以看到foldl的第二个参数就是F(Hd, Accu)的返回结果.


### foldr/3
不说了,跟上面一样,只不过是从右边开始带入.
### map/2
> map(Fun, List1) -> List2

返回List1中每个元素执行Fun后的结果列表,官方doc上说执行顺序是运行时决定,这个和下面说的foreach不一样.
例子:
```erlang
34> lists:map(fun(X) -> X*2 end, [1,2,3,4,5]).            
[2,4,6,8,10]
```
源码:
```erlang
map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) when is_function(F, 1) -> [].
```

### foreach/2

> foreach(Fun, List1) -> ok.

该函数和map很像,区别就是foreach只执行Fun函数却不保存结果,最后返回ok.
源码:
```erlang
foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(F, []) when is_function(F, 1) -> ok.
```

这里和map不同的是foreach的递归在执行完前一个元素的Fun函数后才会继续执行一下个元素的Fun函数,这里的执行顺序就是列表的顺序,官方doc上说:`this function is used for its side effects`
我不是很理解具体有什么用...

### keydelete/3
> keydelete(Key, N, TupleList) -> TupleList1

主要用于由{}tuple 组成的列表,删除Tuplelist中第一个第N个位置的值等于Key的tuple,N是tuple中key应该匹配的位置.这个也经常用,比如一个地图中存了{PlayerId, name, str, magic...}之类的tuplelist,如果玩家离开地图,可以让Key为PlayerId,然后keydelete这个玩家的信息,因为PlayerId一般都是唯一的.
例子:
```erlang
37> lists:keydelete(2010211108, 1, [{2010211105, sice, 5},{2010211106, sice, 6},{2010211107, sice, 7},{2010211108, 8}]).
[{2010211105,sice,5},
 {2010211106,sice,6},
 {2010211107,sice,7}]
```
源码:
```erlang
keydelete(K, N, L) when is_integer(N), N > 0 ->
    keydelete3(K, N, L).

keydelete3(Key, N, [H|T]) when element(N, H) == Key -> T;
keydelete3(Key, N, [H|T]) ->
    [H|keydelete3(Key, N, T)];
keydelete3(_, _, []) -> [].
```

### keyfind/3
> keyfind(Key, N, TupleList) -> Tuple | false

大致同上,如字面意思,只不过不delete掉,没找到会返回false,不过这个是BIF,也就是说用c写的,在lists模块看不到实现方法,BIF的效率一般都很高.
例子:
```erlang
38> lists:keyfind(2010211108, 1, [{2010211105, sice, 5},{2010211106, sice, 6},{2010211107, sice, 7}]). 
false
39> lists:keyfind(2010211106, 1, [{2010211105, sice, 5},{2010211106, sice, 6},{2010211107, sice, 7}]).
{2010211106,sice,6}
```

> BIF的源码可以把github上的[erlang/otp](https://github.com/erlang/otp)clone下来
然后在otp/erts/emulator/beam里面可以找到BIF的c代码,lists里面的BIF写在erl_bif_lists.c中
以下是keyfind的源码,以后的BIF代码就不贴了,感兴趣可以自己去找,不是那么好懂.

```c
keyfind(int Bif, Process* p, Eterm Key, Eterm Pos, Eterm List)
{
    int max_iter = 10 * CONTEXT_REDS;
    Sint pos;
    Eterm term;

    if (!is_small(Pos) || (pos = signed_val(Pos)) < 1) {
	BIF_ERROR(p, BADARG);
    }

    if (is_small(Key)) {
	double float_key = (double) signed_val(Key);

	while (is_list(List)) {
	    if (--max_iter < 0) {
		BUMP_ALL_REDS(p);
		BIF_TRAP3(bif_export[Bif], p, Key, Pos, List);
	    }
	    term = CAR(list_val(List));
	    List = CDR(list_val(List));
	    if (is_tuple(term)) {
		Eterm *tuple_ptr = tuple_val(term);
		if (pos <= arityval(*tuple_ptr)) {
		    Eterm element = tuple_ptr[pos];
		    if (Key == element) {
			return term;
		    } else if (is_float(element)) {
			FloatDef f;

			GET_DOUBLE(element, f);
			if (f.fd == float_key) {
			    return term;
			}
		    }
		}
	    }
	}
    } else if (is_immed(Key)) {
	while (is_list(List)) {
	    if (--max_iter < 0) {
		BUMP_ALL_REDS(p);
		BIF_TRAP3(bif_export[Bif], p, Key, Pos, List);
	    }
	    term = CAR(list_val(List));
	    List = CDR(list_val(List));
	    if (is_tuple(term)) {
		Eterm *tuple_ptr = tuple_val(term);
		if (pos <= arityval(*tuple_ptr)) {
		    Eterm element = tuple_ptr[pos];
		    if (Key == element) {
			return term;
		    }
		}
	    }
	}
    } else {
	while (is_list(List)) {
	    if (--max_iter < 0) {
		BUMP_ALL_REDS(p);
		BIF_TRAP3(bif_export[Bif], p, Key, Pos, List);
	    }
	    term = CAR(list_val(List));
	    List = CDR(list_val(List));
	    if (is_tuple(term)) {
		Eterm *tuple_ptr = tuple_val(term);
		if (pos <= arityval(*tuple_ptr)) {
		    Eterm element = tuple_ptr[pos];
		    if (CMP(Key, element) == 0) {
			return term;
		    }
		}
	    }
	}
    }

    if (is_not_nil(List))  {
	BIF_ERROR(p, BADARG);
    }
    return am_false;
}
```

哎,跟这c代码比,还是erlang看起来舒服,简单.

### keyserach/3
> keysearch(Key, N, TupleList) -> {value, Tuple} | false

与keyfind的功能相同,只不过返回值多了个value的标签,同样是BIF,lists.erl中没有源码,这个函数存在是为了向后兼容,keyfind用的更多些,但是keyfind是在R13B版本引入的,较老的版本用的是keysearch.

### keymember/3
> keymember(Key, N, Tuplelist) -> boolean()

功能和以上大致一样,也是BIF,只不过返回是true或false.

### keymap/3
> keymap(Fun, N, Tuplelist) -> Tuplelist2

用于将每个tuple的第N个元素替换为Fun(第N个元素)的执行结果.
例子:
```erlang
40> lists:keymap(fun(X) -> atom_to_list(X) end, 2, [{1, john, boy},{2, aya, girl}, {3, cameron, robot}]).
[{1,"john",boy},{2,"aya",girl},{3,"cameron",robot}]
```
源码:
```erlang
keymap(Fun, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, Fun(element(Index, Tup)))|keymap(Fun, Index, Tail)];
keymap(Fun, Index, []) when is_integer(Index), Index >= 1, 
                            is_function(Fun, 1) -> [].
```

setelement从字面就可以看出是将Tup的 Index内容替换为Fun(element(Index, Tup)). 而,element就是返回Tup中第Index的值,这些东西都浓缩到一句代码实现,所以说erlang很有意思.

### keymerge/3
> keymerge(N, TupleList1, TupleList2) -> TupleList3

将TupleList1和TupleList2合并并排序,其中N用来制定用Tuple中的第N元素为依据排序,注意,tuplelist1和tuplelist2在执行keymerge前需要是已经按N排好序,否则顺序可能不正确,如果tuplelist1有和tuplelist2中有第N元素相同的tuple,则Tuplelist1的在前面.
例子:
```erlang
42> lists:keymerge(2, [{a,1},{c,3}],[{b,2},{d,4}]).
[{a,1},{b,2},{c,3},{d,4}]
%顺序出错
43> lists:keymerge(2, [{a,1},{c,3}],[{d,4},{b,2}]).     
[{a,1},{c,3},{d,4},{b,2}]  
%tuplelist1的在前面
44> lists:keymerge(2, [{a,1},{c,3}],[{e,3},{d,4}]).
[{a,1},{c,3},{e,3},{d,4}]
```

源码不放了,这个比较复杂,太长了,感兴趣的自己去找吧.

### keyreplace/4
> keyreplace(Key, N, TupleList1, NewTuple) -> TupleList2

如字面意思,用于替换,将TupleList1中第N个值等于Key的tuple替换为NewTuple.只替换第一个.
例子:
```erlang
46> lists:keyreplace(name2, 2, [{player1, name1, career1},{player2, name2, career2},{player3, name3, career3}], {player4, name4, career4}).
[{player1,name1,career1},
 {player4,name4,career4},
 {player3,name3,career3}]
```
源码:
```erlang
keyreplace(K, N, L, New) when is_integer(N), N > 0, is_tuple(New) ->
    keyreplace3(K, N, L, New).

keyreplace3(Key, Pos, [Tup|Tail], New) when element(Pos, Tup) == Key ->
    [New|Tail];
keyreplace3(Key, Pos, [H|T], New) ->
    [H|keyreplace3(Key, Pos, T, New)];
keyreplace3(_, _, [], _) -> [].
```

### keysort/2
> keysort(N, TupleList1) -> TupleList2.

example:
```erlang
47> lists:keysort(1,[{3,name3},{2, name2}, {4,name4},{1,name1}]).
[{1,name1},{2,name2},{3,name3},{4,name4}]
```

源码不贴了,这个函数的源码很长.
### keystore/4
> keystore(Key, N, TupleList1, NewTuple) -> TupleList2.

功能和keyreplace差不多,只不过当TupleList1中没有与Key相同的Tuple时,keyreplace返回的是原来的TupleList,而keystore是将NewTuple append在原TupleList上.
例子;
```erlang
48> lists:keyreplace(name5, 2, [{player1, name1, career1},{player2, name2, career2},{player3, name3, career3}], {player4, name4, career4}).
[{player1,name1,career1},
 {player2,name2,career2},
 {player3,name3,career3}]
49> lists:keystore(name5, 2, [{player1, name1, career1},{player2, name2, career2},{player3, name3, career3}], {player4, name4, career4}).  
[{player1,name1,career1},
 {player2,name2,career2},
 {player3,name3,career3},
 {player4,name4,career4}]
```

### keytake/3
> keytake(Key, N, TupleList1) -> {value, Tuple, TupleList2} | false

与大多数key打头的函数相同,字面意思是从TupleList中拿出第N个值和Key相等的Tuple,如果有的话,返回{value, Tuple, TupleList2},Tuple是想的Tuple,TupleList2是去掉Tuple后的新的TupleList.如果没找到则返回false.
example:
```erlang
50> lists:keytake(name2, 2, [{player1, name1, career1},{player2, name2, career2},{player3, name3, career3}]).                            
{value,{player2,name2,career2},
       [{player1,name1,career1},{player3,name3,career3}]}
```
源码:
```erlang
keytake(Key, N, L) when is_integer(N), N > 0 ->
    keytake(Key, N, L, []).

keytake(Key, N, [H|T], L) when element(N, H) == Key ->
    {value, H, lists:reverse(L, T)};
keytake(Key, N, [H|T], L) ->
    keytake(Key, N, T, [H|L]);
keytake(_K, _N, [], _L) -> false.
```

### last/1
> last(List) -> Last

返回List中最后一个元素.
源码:
```erlang
last([E|Es]) -> last(E, Es).

last(_, [E|Es]) -> last(E, Es);
last(E, []) -> E.
```

### mapfoldl/3
> mapfoldl(Fun, Acc0, List1) -> {List2, Acc1}.

官方的解释就是同时具有map和foldl的功能.直白点就是map完成List1->List2的变化,foldl完成 Acc0 -> Acc1的变化.
因为foldl只能返回一个由List1得到的记过Acc1,而不能多List1本身得到新的List2,而map不能得到Acc1,却能完成List1到List2的变化.所以mapfoldl完成了两者的功能.看源码会比较清晰的理解下.
源码:
```erlang
-spec mapfoldl(Fun, Acc0, List1) -> {List2, Acc1} when
      Fun :: fun((A, AccIn) -> {B, AccOut}),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[],Accu}.
```
注意,这里面的Fun返回的是{B, AccOut}的形式,而foldl的Fun是`      Fun :: fun((Elem :: T, AccIn) -> AccOut),
`这里是最本质的区别.结合例子说下
```erlang
52> lists:mapfoldl(fun(X, Sum) -> {X*2, X + Sum} end, 0, [1,2,3,4,5]).
{[2,4,6,8,10],15}
```

首先第一轮得到的{R, Accu1} = {2, 1},而{Rs, Accu2}需要递归执行mapfoldl,那么第二轮得到一个{R',Accu1'} = {4, 3},我们每轮能确切知道的只有{R, Accu1},但是mapfoldl要返回的是{[R|Rs],Accu2},所以需要一直递归下去,那么第三轮得到{6, 6},第四轮{8, 10},第五轮得到{10, 15},这时{10, 15}仍然是{R, Accu1},但是接下来由于Tail变成了空列表[],根据模式匹配,这次执行的是第二个mapfoldl函数,那么相当于执行mapfoldl{F, 15, []),得到最底层的{Rs, Accu2} = {[], 15},那么返回的{[R|Rs], Accu2}为{[10|[]], 15}.一直往上递归回去,结果得到{[2,4,6,8,10], 15}.

### mapfoldr/3
与上面的执行结果一样,只不过foldr是从右边开始,不影响List2的顺序.

### max/1
> max(List) -> Max

返回第一个这个List中的最大数.
例子:
```erlang
54> lists:max([a,b,c]).
c
55> lists:max([100,b,c]).
c
56> lists:max([{100},b,c]).
{100}
57> lists:max([{100},b,c,[90]]).
"Z"
58> lists:max([{100},b,c,[2]]). 
[2]
59> lists:max([{100},b,c,[2],<<2>>]).
<<2>>
```

这里注意下不同类型之间的比较顺序
> number &lt; atom < reference < fun < port < pid < tuple < list < binary

源码:
```erlang
max([H|T]) -> max(T, H).

max([H|T], Max) when H > Max -> max(T, H);
max([_|T], Max)              -> max(T, Max);
max([],    Max)              -> Max.
```

### member/2
> member(Elem, List) -> boolean()

如果Elem是List中的元素就返回true, 否则返回false.BIF.源码是C写的.不贴了.
### merge/1/2/3以及merge3/3
> merge(ListOfLists) -> List1.
merge(List1, List2) -> List3.
merge(Fun, List1, List2) -> List3.
merge3(List1, List2, List3) -> List4

暂且叫归并排序吧,merge/1,merge/2完成的就是合并并排序,顺序由小到大,merge3/3只不过可以合并3个列表.merge/3可以依据Fun更改顺序,排序依据是Fun(A, B)如果返回true,那么就是A在B的前面.
举几个例子:
```erlang
60> lists:merge([[1],[4],[2,4],[3]]).
[1,2,3,4,4]
61> lists:merge([2,4,6],[1,3]).      
[1,2,3,4,6]
62> lists:merge(fun(A,B) -> A > B end, [5,4,2],[8,3,1]).
[8,5,4,3,2,1]
63> lists:merge(fun(A,B) -> A < B end, [5,4,2],[8,3,1]).
[5,4,2,8,3,1]
64> lists:merge(fun(A,B) -> A < B end, [2,4,5],[1,3,8]).
[1,2,3,4,5,8]
66> lists:merge3([2,4,6],[1,3],[2,3,5,7]).
[1,2,2,3,3,4,5,6,7]
```

所有的排序都要求每个子List在排序前都是排好序的,如63行,由于子列表没有排好序,所以排序结果不正确.
merge的源码比较多而且杂,感兴趣自己去lists.erl里面看.

### min/1
> min(List) -> Min

与max相反.

### nth/2
> nth(N, List) -> Elem

返回第List的第N个元素.
例子:
```erlang
67> lists:nth(2,["haha",atom,2,<<"binary">>]). 
atom
```
源码:
```erlang
nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T).
```

### nthtail/2
> nthtail(N, List) -> Tail

返回List的第N个尾列表.
例子:
```erlang
68> lists:nthtail(0,[1,2,3,4]).
[1,2,3,4]
69> lists:nthtail(1,[1,2,3,4]).
[2,3,4]
70> lists:nthtail(2,[1,2,3,4]).
[3,4]
71> lists:nthtail(4,[1,2,3,4]).
[]
```
源码:
```erlang
nthtail(1, [_|T]) -> T;
nthtail(N, [_|T]) when N > 1 ->
    nthtail(N - 1, T);
nthtail(0, L) when is_list(L) -> L.
```

### partition/2
> partition(Fun, List1) -> {List1, List2}.

字面意思是区分开,将List1依据Fun返回的true和false分为两个列表.
例子:
```erlang
73> lists:partition(fun(X) -> X>2 end, [1,2,3,4]).
{[3,4],[1,2]}
```
源码
```erlang
partition(Pred, L) ->
    partition(Pred, L, [], []).

partition(Pred, [H | T], As, Bs) ->
    case Pred(H) of
	true -> partition(Pred, T, [H | As], Bs);
	false -> partition(Pred, T, As, [H | Bs])
    end;
partition(Pred, [], As, Bs) when is_function(Pred, 1) ->
    {reverse(As), reverse(Bs)}.
```

### prefix/2
> prefix(List1, List2) -> boolean().

如果List1是List2的前缀则返回true,否则返回false
例子:
```erlang
74> lists:prefix([1,2],[1,2,3,4]).                
true
75> lists:prefix([1,2],[1,3,4]).  
false
```
源码很好理解:
```erlang
prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) when is_list(List) -> true;
prefix([_|_], List) when is_list(List) -> false.
```

如果第一元素都是X,则递归执行prefix,如果List1为[]了,那么返回true,如果遇到不相同的元素了就返回false.

### reverse/1/2
> reverse(List1) -> List2.
reverse(List1, Tail) -> List2.

reverse/1返回的是逆序的List1. reverse/2返回的是逆序的List1并且append了Tail.
例子:
```erlang
76> lists:reverse([1,2,3,4]).
[4,3,2,1]
77> lists:reverse([1,2,3,4],[5,6,7]).
[4,3,2,1,5,6,7]
```
源码:
```erlang
-spec reverse(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]).
```

注意这里比较神奇的是reverse/1调用的是reverse/2,我一直以为reverse/1是BIF,其实reverse/2才是BIF,而且reverse/2在各种merge函数中多次调用,调用reverse/2的次数非常多,估计所以才将reverse/2写成了BIF吧.

### seq/2/3
> seq(From, To) -> List1
seq(From, To, Incr) -> List2

用于生成From到To的列表,Incr是递增的值.这个函数通常只用于最简单的功能,但是还是需要注意一下,这个函数报错的条件比较苛刻.官方给的说明是在一下会发生error错误.
1. To < From - Incr 并且Incr是正数.
2. To > From - Incr 并且Incr是负数.
3. Incr为0时,From 不等于 To
并且官方给出
> length(lists:seq(From, To)) == To-From+1
length(lists:seq(From, To, Incr)) == (To-From+Incr) div Incr

涵盖了上面3种情况,因为length不能是负数,其实只要看***To-From+Incr与Incr相除***是正数或者0就不会报错.
以下是一些例子:
```erlang
78> lists:seq(1,4).
[1,2,3,4]
79> lists:seq(3,1).
** exception error: no function clause matching lists:seq(3,1) (lists.erl, line 177)
80> lists:seq(3,1,-1).
[3,2,1]
81> lists:seq(3,1,1). 
** exception error: no true branch found when evaluating an if expression
     in function  lists:seq/3 (lists.erl, line 198)
82> lists:seq(1,0,1).
[]
83> lists:seq(10,6,1).
** exception error: no true branch found when evaluating an if expression
     in function  lists:seq/3 (lists.erl, line 198)
84> lists:seq(10,6,4).
[]
85> lists:seq(10,6,5).
[]
86> lists:seq(10,6,3).
** exception error: no true branch found when evaluating an if expression
     in function  lists:seq/3 (lists.erl, line 198)
87> lists:seq(1,4).
[1,2,3,4]
88> lists:seq(1,0).
[]
89> lists:seq(3,0,-1).
[3,2,1,0]
90> lists:seq(10,6,4).
[]
91> lists:seq(10,6,3).
** exception error: no true branch found when evaluating an if expression
     in function  lists:seq/3 (lists.erl, line 198)
92> lists:seq(6,8,-3).
[]
93> lists:seq(6,10,-3).
** exception error: no true branch found when evaluating an if expression
     in function  lists:seq/3 (lists.erl, line 198)
94> lists:seq(1,2,0).  
** exception error: no true branch found when evaluating an if expression
     in function  lists:seq/3 (lists.erl, line 198)
```

### sort/1/2
> sort(List1) -> List2.
sort(Fun, List1) -> List2

排序函数,应熟记不同类型之间的大小顺序,如例子:
```erlang
95> Ref = make_ref().
#Ref<0.0.0.358>
96> Fun = fun(X) -> X end.
#Fun<erl_eval.6.111823515>
97> {ok, Port}=gen_tcp:listen(8011,[]).
{ok,#Port<0.619>}
98> Pid = spawn(lists, seq, [1,100]).
<0.151.0>
99> lists:sort([<<2>>,[3],{4},Pid,Port,Fun,Ref,atom,5]).
[5,atom,#Ref<0.0.0.358>,#Fun<erl_eval.6.111823515>,
 #Port<0.619>,<0.151.0>,
 {4},
 [3],
 <<2>>]
```

我在max函数提到过这个顺序,上面的结果也是按照这个顺序来的
> number &lt; atom < reference < fun < port < pid < tuple < list < binary

sort/2大致如merge/3,其中Fun可以用来改变默认的由小到大.

### split/2
> split(N, List1) -> {List2, List3}

分割函数,将List1分割成前N个元素List2,和剩下的元素List3
```erlang
100> lists:split(3,[1,2,3,4,5]). 
{[1,2,3],[4,5]}
```
源码:
```erlang
-spec split(N, List1) -> {List2, List3} when
      N :: non_neg_integer(),
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

split(N, List) when is_integer(N), N >= 0, is_list(List) ->
    case split(N, List, []) of
	{_, _} = Result -> Result;
	Fault when is_atom(Fault) ->
	    erlang:error(Fault, [N,List])
    end;
split(N, List) ->
    erlang:error(badarg, [N,List]).

split(0, L, R) ->
    {lists:reverse(R, []), L};
split(N, [H|T], R) ->
    split(N-1, T, [H|R]);
split(_, [], _) ->
    badarg.
```

### splitwith/2
splitwith(Fun, List) -> {List1, List2}

Fun决定切割的规则,List1是在遇到Fun(X)为false之前的元素,List2是第一个Fun返回为false之后的列表.
例子:
```erlang
101> lists:splitwith(fun(X) -> is_atom(X) end, [a,b,c,2,d,e,4]).
{[a,b,c],[2,d,e,4]}
```
源码:
```erlang
-spec splitwith(Pred, List) -> {List1, List2} when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      List1 :: [T],
      List2 :: [T],
      T :: term().

splitwith(Pred, List) when is_function(Pred, 1) ->
    splitwith(Pred, List, []).

splitwith(Pred, [Hd|Tail], Taken) ->
    case Pred(Hd) of
	true -> splitwith(Pred, Tail, [Hd|Taken]);
	false -> {reverse(Taken), [Hd|Tail]}
    end;
splitwith(Pred, [], Taken) when is_function(Pred, 1) ->
    {reverse(Taken),[]}.
```

### sublist/2/3
> sublist(List1, Len) -> List2
sublist(List1, Start, Len) -> List2

返回前List中前Len个元素,Len可以大于List的长度,这样整个List会返回.
Start可以决定起始位置.
例子:
```erlang
102> lists:sublist([1,2,3,4],2).                                
[1,2]
103> lists:sublist([1,2,3,4],2,2).
[2,3]
104> lists:sublist([1,2,3,4],5).  
[1,2,3,4]
105> lists:sublist([1,2,3,4],5,1).
[]
```
源码
```erlang
sublist(List, L) when is_integer(L), is_list(List) ->
    sublist_2(List, L).

sublist_2([H|T], L) when L > 0 ->
    [H|sublist_2(T, L-1)];
sublist_2(_, 0) ->
    [];
sublist_2(List, L) when is_list(List), L > 0 ->
    [].


sublist(List, S, L) when is_integer(L), L >= 0 ->
    sublist(nthtail(S-1, List), L).
```

### subtract/2
> subtract(List1, List2) -> List3

规则是这样的:对于List2中的每一个元素,在List1第一个与这个元素相等的元素被删掉,而成为List3.
例如:
```erlang
107> lists:subtract([3,2,1,3,4,1],[1,3,5]). 
[2,3,4,1]
```
源码:
```erlang
-spec subtract(List1, List2) -> List3 when
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

subtract(L1, L2) -> L1 -- L2.
```

很明显这个函数在最坏情况下,有length(List1)*length(List2)的复杂度,代价很高.官方给的建议是
> Using ordered lists and ordsets:subtract/2 is a much better choice if both lists are long.

使用ordset中的subtract/2,前提是这两个List是排好序的.
源码:
```erlang
-spec subtract(Ordset1, Ordset2) -> Ordset3 when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_),
      Ordset3 :: ordset(_).

subtract([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    [E1|subtract(Es1, Set2)];
subtract([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    subtract(Set1, Es2);
subtract([_E1|Es1], [_E2|Es2]) ->		%E1 == E2
    subtract(Es1, Es2);
subtract([], _) -> [];
subtract(Es1, []) -> Es1.
```

### suffix/2
> suffix(List1, List2) -> boolean().

如同prefix,如果List1是List2的后缀则返回true,否则返回false.
例子:
```erlang
108> lists:suffix([3,1],[1,2,3,1]).                             
true
```
源码:
```erlang
suffix(Suffix, List) ->
    Delta = length(List) - length(Suffix),
    Delta >= 0 andalso nthtail(Delta, List) =:= Suffix.
```

### sum/1
> sum(List) -> Sum

得到一个List的和,注意List应为数字组成.否则抛错.
源码:
```erlang
sum(L)          -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.
```

### takewhile/2
> takewhile(Pred, List1) -> List2

从List1中取出满足Pred为true的元素,直到遇到第一个false.也就是取出满足Pred的前缀.
例子:
```erlang
109> lists:takewhile(fun(A) -> A < 3 end, [2,1,3,2,1]).
[2,1]
```
源码:
```erlang
takewhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> [Hd|takewhile(Pred, Tail)];
	false -> []
    end;
takewhile(Pred, []) when is_function(Pred, 1) -> [].
```

### ukeymerge/3
> ukeymerge(N, TupleList1, TupleList2) -> TupleList3

类似于keymerge,但是u的意思是unique的意思,就是当TupleList1和TupleList2中有相同key的Tuple时将会删掉TupleList2中的Tuple.而且要求TupleList1和TupleList2在执行函数前自己本身不含有相同key的Tuple,否则结果会只删除一个TupleLIst2中的Tuple.结果不如人意.
例子:
```erlang
1> lists:ukeymerge(2,[{1,10},{2,20},{5,50}],[{2,20},{4,40}]).
[{1,10},{2,20},{4,40},{5,50}]
2> lists:keymerge(2,[{1,10},{2,20},{5,50}],[{2,20},{4,40}]). 
[{1,10},{2,20},{2,20},{4,40},{5,50}]
3> lists:ukeymerge(2,[{1,10},{2,20},{5,50}],[{2,20},{3,20},{4,40}]).
[{1,10},{2,20},{3,20},{4,40},{5,50}]
4> lists:ukeymerge(2,[{1,10},{2,20},{5,50}],[{2,20},{2,20},{4,40}]).
[{1,10},{2,20},{2,20},{4,40},{5,50}]
```

可以看到keymerge和ukeymerge的差别,而且第3行是在TupleList2中有相同Key的Tuple,倒是结果中也有相同的key,这时因为只删除了第一个与TupleList1中相同的Tuple即{2, 20}.
源码不贴了,凡是根merge有关的,都复杂而且长..

### ukeysort/2
> ukeysort(N, TupleList1) -> TupleList2

与keysort差不多,同样u也是唯一的意思,只保留一个keyTuple,如以下例子:
```erlang
5> lists:ukeysort(2, [{1,10},{3,30},{4,40},{2,20},{5,20}]).         
[{1,10},{2,20},{3,30},{4,40}]
6> lists:keysort(2, [{1,10},{3,30},{4,40},{2,20},{5,20}]). 
[{1,10},{2,20},{5,20},{3,30},{4,40}]
```

### umerge/1/2/3以及 umerge3/3
参加merge,只不过只保留一个相同的key.

### uzip/1以及uzip3/1
> unzip(List1) -> {List2, List3}
unzip3(List1) -> {List2, List3, List4}

用于分离TupleList中的每个Tuple的第一个元素进入List2,第二个元素进入List3.unzip3适用于3-Tuple的情况.
例子:
```erlang
7> lists:unzip([{10210221, fubowen},{10210222, yuyouqi}, {10201223, xuyue}]).
{[10210221,10210222,10201223],[fubowen,yuyouqi,xuyue]}
8> lists:unzip3([{10210221, fubowen, 1},{10210222, yuyouqi, 1}, {102101001, aya, 0}]).
{[10210221,10210222,102101001],
 [fubowen,yuyouqi,aya],
 [1,1,0]}
```
源码:
```erlang
unzip(Ts) -> unzip(Ts, [], []).

unzip([{X, Y} | Ts], Xs, Ys) -> unzip(Ts, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {reverse(Xs), reverse(Ys)}.

unzip3(Ts) -> unzip3(Ts, [], [], []).

unzip3([{X, Y, Z} | Ts], Xs, Ys, Zs) ->
    unzip3(Ts, [X | Xs], [Y | Ys], [Z | Zs]);
unzip3([], Xs, Ys, Zs) ->
    {reverse(Xs), reverse(Ys), reverse(Zs)}.
```

### zip/2以及zip3/3
> zip(List1, List2) -> List3
zip3(List1, List2, List3) -> List4

就是unzip的反过程.

### usort/1和usort/2
> usort(List1) -> List2
usort(Fun, List1) -> List2

见sort,只不过只保留一个相同的值.
例子:
```erlang
10> lists:usort([1,3,2,4,2,5,2,2,2]).
[1,2,3,4,5]
```

### zipwith/3以及zipwith3/4
> zipwith(Combine, List1, List2) -> List3
zipwith3(Combine, List1, List2, List3) -> List4

功能大致同zip/2,zip3/3相同,只不过多了个Combine Function,这个Combine决定了如何合并.
例子:
```erlang
11> lists:zipwith(fun(X,Y) -> X+Y end, [1,2,3], [2,3,4]).
[3,5,7]
```

至此,lists模块的函数全都介绍完毕,啊,好累,休息会.

