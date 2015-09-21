title: gitnote
date: 2015-09-19 12:22:10
tags: git
---
# git 

##startup

配置文件  
   1. /etc/.gitconfig 最顶级
   2. /home/$USERNAME/.gitconfig 用户设定
   3. project/.gitconfig 当前项目
下面会把上面的设定覆盖.

`git config --global user.name "XXX"`
修改/home/username config
`git config (--local) user.name "YY" `
修改当前项目下的gitconfig
`git config --system user.name "ZZZ"`
系统通用配置文件

`git config --global|system|local --get user.name  -> XXX`
`git config user.name ` 简写

<!-- more -->

##2nd
`git init` -> generate .git file
`git add` -> 可以跟踪新文件，也可以将修改文件放入暂存区

gitignore遵循glob匹配
  1. `#`之后是注释
  2. 名字后面是/表示目录
  3. !表示取反
  4. *匹配一个或多个字符
  5. [abc]匹配ａｂｃ中任意一个字符
  6. ？表示任意一个字符
  7. [0-9], [a-z] 两个字符之间
  
`git diff` 比较暂存和未暂存的区别
`git diff --cached`　比较暂存和上次提交的区别

`git commit` 提交暂存区
`git commit -a` 跳过git add 步骤，跳过暂存区, 直接提交
`git rm` 删除跟踪的某个文件,如果文件已在暂存区需要`-f`,该命令同时删除文件
`git rm --cached` 不删除文件,只删除暂存去或者跟踪
`git mv` 重命名

`git log`: `-p` 显示差异 `-2` 最近两次log `--name-status` 显示文件状态 `--relative-data`显示诸如几周前这种相对时间 `--pretty=oneline|short|full|fuller|format`定制显示格式
`git log --since=2.weeks` 两周以内 `--since="2015-08-01"` 指定日期之后 `--until="2015-08-01"` 指定日期之前
`git log　--author=XXX --grep= sometest --all-match(与关系)`
###gitk　
git log 图形化界面

`git --amend` 修改上一次提交,如果上一次commit落下了一些文件没有提交可以在commit之后 git add等操作之后amend，之产生一次提交

`git reset HEAD <filename>` 移除暂存区
`git checkout --<filename>` 撤销为加入暂存区的文件的修改

`git remote -v` 显示远程仓库
`git tag (-l "0.4.*|[0-9]")` 显示特定标签 glob匹配

分支的原理
[pro git](http://git.oschina.net/progit/3-Git-%E5%88%86%E6%94%AF.html#3.1-%E4%BD%95%E8%B0%93%E5%88%86%E6%94%AF)
建立分支步骤：
`git branch (-v)` 列出所有分支
`git branch branchname` (建立） -> `git checkout branchname` (跳到对应branch).
以上等同于
`git checkout -b branchname`
`git branch -d branchname` 删除分支
`git branch --merged(--no-merged)`

##搭建git服务器
以下例子是在我自己的linode的vps上搭的步骤
由于买的vps自带ssh-server. 不需要安装ssh-server
如果是自己的需要执行(ubuntu下)
```
sudo apt-get install openssh-server
sudo /etc/init.d/ssh start(sshd start)
sudo apt-get install git
```
centos用yum安装
安装ssh_server. 客户端默认安装了ssh_client.
安装完成后可以通过shell
```
ssh username@ip
```

的方式登陆远程服务器
如果提示connect to host xxx.xxx.xxx.xxxport 22: Connection refused
说明openssh_server没安装成功或者没有start

```
youthy@youthy:~$ ssh  root@XXX.XXX.XXX.XXX
root@xxx.xxx.xxx.xxx's password: 
Last login: Sat Sep 19 07:09:43 2015 from xxx.xxx.xxx.xxx
[root@li1166-59 ~]# adduser test
[root@li1166-59 ~]# passwd test
更改用户 test 的密码 。
新的 密码：
重新输入新的 密码：
passwd： 所有的身份验证令牌已经成功更新。
[root@li1166-59 ~]# su test
[test@li1166-59 root]$ 
```

输入exit退出ssh
接下来创建需要传到服务器上的文件(例子用是博客的源文件)
```
git clone --bare https://github.com/youthy/youthy.github.io test.git
youthy@youthy:~$ ls test.git/
branches  config  description  HEAD  hooks  info  objects  packed-refs  refs
```

--bare 用来创建裸版本库。`git clone --bare REPOS NAME.git` REPOS 可以为本地文件
```
youthy@youthy:~$ scp -r test.git test@xxx.xxx.xxx.xxx:/home/test
test@xxx.xxx.xxx.xxx's password: 
HEAD                                                                                                                                                                       100%   23     0.0KB/s   00:00    
packed-refs                                                                                                                                                                100%   98     0.1KB/s   00:00    
prepare-commit-msg.sample                                                                                                                                                  100% 1239     1.2KB/s   00:00    
pre-rebase.sample                                                                                                                                                          100% 4898     4.8KB/s   00:00    
commit-msg.sample                                                                                                                                                          100%  896     0.9KB/s   00:00    
post-update.sample                                                                                                                                                         100%  189     0.2KB/s   00:00    
pre-applypatch.sample                                                                                                                                                      100%  398     0.4KB/s   00:00    
pre-push.sample                                                                                                                                                            100% 1352     1.3KB/s   00:00    
applypatch-msg.sample                                                                                                                                                      100%  452     0.4KB/s   00:00    
pre-commit.sample                                                                                                                                                          100% 1642     1.6KB/s   00:00    
update.sample                                                                                                                                                              100% 3611     3.5KB/s   00:00    
exclude                                                                                                                                                                    100%  240     0.2KB/s   00:00    
description                                                                                                                                                                100%   73     0.1KB/s   00:00    
pack-b7cb7743297b04547b0401b940914333ed2d513b.pack                                                                                                                         100% 3025KB 504.2KB/s   00:06    
```

此时所有安装git的用户可以通过输入密码的方式`git clone test@XXX.XXX.XXX.XXX:/home/test/test.git`
克隆这个库和修改这个库，
####免密码
为了在不用每次提交都输入密码
需要将自己的.ssh/id_rsa.pub 公钥加入到test/.ssh/authorize_keys 中，如果没有需要自己建立。没有公钥的话，用
`ssh-kegen (-t rsa|dsa)` 生成(默认时dsa加密方式)
但是需要注意以下两点
***authorize_keys***需要权限是600 `chmod 600 test/.ssh/authorize_keys`
***将自己的私钥***加入到ssh_agent中：在客户端执行`ssh-add   ~/.ssh/id_rsa`
否则会出现`Agent admitted failure to sign using the key.` 无法免密码登陆

####安全措施：
以上完成之后参与者都可以通过`ssh test@XXX.XXX.XXX.XXX`登陆我的vps，可以通过以下方式
```
[root@li1166-59 ~]# which git-shell
/usr/bin/git-shell
[root@li1166-59 ~]# vim /etc/passwd
```

passwd文件内容如下
```
...
NTP:X:38:38::/ETC/NTP:/SBIN/NOLOGIN
test:x:500:500::/home/git:/bin/bash
...
```

将 test用户的bin/bash改为git-shell或者去掉
```
test:x:500:500::/home/git:/usr/bin/git-shell
```

这样就无法通过test登陆服务器
更多参考
[pro git](http://git.oschina.net/progit/4-%E6%9C%8D%E5%8A%A1%E5%99%A8%E4%B8%8A%E7%9A%84-Git.html#)
