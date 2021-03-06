title: Regex 正则表达式
date: 2016-05-06 15:56:24
tags: [erlang, vim]

---

1. "regex" 精确的匹配 regex这个词
2. 上面可能会把"regex2"这种词的前面也匹配到。此时需要"\bregex\b" 

<!--more-->
> \b是正则表达式规定的一个特殊代码（好吧，某些人叫它元字符，metacharacter），代表着单词的开头或结尾，也就是单词的分界处。虽然通常英文的单词是由空格，标点符号或者换行来分隔的，但是\b并不匹配这些单词分隔字符中的任何一个，它只匹配一个位置。

**erlang中需要用`\\` 代表 `\`**
> The Erlang literal syntax for strings uses the "\" (backslash) character as an escape code. You need to escape backslashes in literal strings, both in your code and in the shell, with an additional backslash, i.e.: "\\".

```
14> re:run("hey hi hihi", "hi", [global]).
{match,[[{4,2}],[{7,2}],[{9,2}]]}
15> re:run("hey hi hihi", "\\bhi\\b", [global]).
{match,[[{4,2}]]}
```

3. `.` 表示匹配出了换行的任意字符 `*` 表示任意数量。 表示`*`前面的字符可以重复任意个 比如".*", "h*", "2*" 匹配任意字符，任意个h，任意个2

4. `\d` 匹配数字。 `"4\d\d\d\d\d\d\d"` 匹配4开头的8位数字。也可以写成`"4\d{7}"`  "\d{7, 10}" 表示7到10个不等
\d在erlang中同样需要换成`\\d`


>.	匹配除换行符以外的任意字符
\w	匹配字母或数字或下划线或汉字
\s	匹配任意的空白符
\d	匹配数字
\b	匹配单词的开始或结束
^	匹配字符串的开始
$	匹配字符串的结束


> 表2.常用的限定符
代码/语法	说明
*	重复零次或更多次
+	重复一次或更多次
?	重复零次或一次
{n}	重复n次
{n,}	重复n次或更多次
{n,m}	重复n到m次

5. "[aeiou]" 规定一个字符集。 比如"[0-9]"就与"\d"一致。
6. 分组 "(2[0-4]\d){2,3}" 括号扩起来的部分是一组。重复2,3次。
7. 反义
> 表3.常用的反义代码
代码/语法	说明
\W	匹配任意不是字母，数字，下划线，汉字的字符
\S	匹配任意不是空白符的字符
\D	匹配任意非数字的字符
\B	匹配不是单词开头或结束的位置
[^x]	匹配除了x以外的任意字符
[^aeiou]	匹配除了aeiou这几个字母以外的任意字符

8. 后向引用 可以给分组命名 通过"\1"或"\name"再次匹配这个分组
> 表4.常用分组语法
分类	代码/语法	说明
捕获	(exp)	匹配exp,并捕获文本到自动命名的组里
(?<name>exp)	匹配exp,并捕获文本到名称为name的组里，也可以写成(?'name'exp)
(?:exp)	匹配exp,不捕获匹配的文本，也不给此分组分配组号
零宽断言	(?=exp)	匹配exp前面的位置
(?<=exp)	匹配exp后面的位置
(?!exp)	匹配后面跟的不是exp的位置
(?<!exp)	匹配前面不是exp的位置
注释	(?#comment)	这种类型的分组不对正则表达式的处理产生任何影响，用于提供注释让人阅读

9. 懒惰匹配
默认情况下 "h.*p" 匹配最长的开头为h尾部为p的中间为任意个字符的字符串 称贪婪匹配
"happy" => "happ"
如果使用"h.*?p" 将匹配得到的最短的字符串。称为懒惰匹配
"happy" => "hap"
`?`可以放在任意限定符的后面比如
>表5.懒惰限定符
代码/语法	说明
*?	重复任意次，但尽可能少重复
+?	重复1次或更多次，但尽可能少重复
??	重复0次或1次，但尽可能少重复
{n,m}?	重复n到m次，但尽可能少重复
{n,}?	重复n次以上，但尽可能少重复
