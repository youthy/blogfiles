title: linux修改键盘映射capslock为ctrl
date: 2014-07-12 22:36:35
tags: [xmodmap，vim]
categories: 教程
---

> 由于在vim下需要经常使用ctrl键，比如ctrl+]在tags之间跳，ctrl+v进入列可视模式等等，但是由于ctrl键按起来比较麻烦
所以一般将capslock映射为ctrl。

<!--more-->

需要xmodmap支持

首先，安装xmodmap

> sudo apt-get install xmodmap

如果提示没有则安装

> sudo apt-get install x11-xserver-utils

之后在自己的工作目录home里新建一个.xmodmaprc的文件，
里面键入
```
     remove Lock = Caps_Lock
     keysym Caps_Lock = Control_L
     add Control = Control_L
```

保存。

在shell里面cd到xmodmaprc所在目录执行

>  xmodmap .xmodmaprc

使映射生效。

如不生效注销一下应该就可以了
