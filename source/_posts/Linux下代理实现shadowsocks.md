title: 'Linux下代理实现:shadowsocks'
date: 2014-06-28 19:52:47
tags: linux
categories: 教程
---

#linux下的代理:shadowsocks 

---
之前为了玩舰娘,从淘宝上买了个日本ip的ssh,用ssh过了一段翻墙的日子,后来舰娘玩的少了,ssh没怎么用,再后来一上ssh竟然无效了.反正也没多大需求了后来就没管它.再后来当我要用到stackoverflow查阅些东西时,发现竟然加载奇慢.
其实并不是stackoverflow被墙了.而是它调用的一些服务被墙了.

<!--more-->
##测试阶段
打开chrome,我们按ctrl+shift+I调出network这个审查面板(也可以按右键选择"审查元素").然后在地址栏输入`stackoverflow.com`我们可以看到究竟是什么东西加载的这么慢.
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_008.png)
可以看到红名的那个加载项.相比大家心里清楚什么原因....毕竟最近google的服务根本没法用....
所以我需要
> 1. shadowsocks代理客户端
2. 一个shadowsocks账号
3. chrome switchysharp插件

***

##shadowsocks账号
可以从淘宝买,很便宜,3块一个月2G流量,8元一个月40G流量,足够用了.如果没有账号,接下来的部分就不用看了.
##shadowsock安装
我推荐用shadowsock-gui这个图形界面.
[github地址](https://github.com/shadowsocks/shadowsocks-gui)
上面安装方法写的很详细了.可以选择直接DOWNLOAD的方法,下载一个tar.gz的包,然后在终端运行

 `tar -xvzf shadowsocks-gui-0.1.3-linux-ia32.tar.gz(我下载的版本)`

解压出来就可以了.
喜欢鼓捣的人可以用DEVELOP的方法安装.
无论怎么安装,最后进入shadowsock的根目录,直接双击nw这个文件就可以,或者用终端`cd`到所在目录,执行`./nw`都可以打开shadowsock-gui.
界面如下
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_009.png)

> **注意**:如果双击nw无反应,或者`./nw`后报了错:
`error while loading shared libraries: libudev.so.0: can not open shared object  file:No such file or directory`
说明我们的这个库程序找不到.
在终端中执行 `locate libudev.so`
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_011.png)
可以看到libudev.so在lib/i386-linux-gnu下,我们需要执行一下软连接命令
`sudo ln -s /lib/i386-linux-gnu/libudev.so.1 /lib/i386-linux-gnu/libudev.so.0`
这样nw就可以正常运行了.

****

##switchysharp
switchysharp的教程太多了,不管使用ssh还是shadowssocks,或者是goagent等各种代理都要用到它.教程一搜一大堆.
这里就不多说,仅给出我的设置.
在shadowsocks里面save好账号信息后,在chrome的switchysharp里做如下设置.
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_012.png)
主要将对应的端口写成shadowsocks的本地端口(我的是1080).
如果使用自动切换模式需要改下下面的自动切换规则.我把googleapis和google-analytics加入了进去.
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_013.png)
顺便把stackoverflow和另外一个stackoverflow调用的但是耗时长的网址加了进去
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_014.png)
保存后就基本完工了.
##我们可以看到googleapis的加载速度一下子提升了很多
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_015.png)
可以顺畅了浏览stackoverflow了,
顺便玩玩舰娘...

