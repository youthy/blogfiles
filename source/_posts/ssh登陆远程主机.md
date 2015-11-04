title: ssh登陆远程主机
date: 2015-11-04 11:36:41
tags: linux
---
###方法一：
将.ssh/id_rsa.pub内容加入到远程主机**.ssh/authorized_keys**下
```
youthy@youthy:~$ scp .ssh/id_rsa.pub user@remote_host:~
输入密码
ssh user@remote_host
输入密码
[root@li1166-59 ~] cat id_rsa.pub >> .ssh/authorized_keys
```

注意文件是**authorized_keys** 不是authorize_keys

###方法二:
```
ssh-copy-id -i .ssh/id_rsa.pub user@remote_host
```

将id_rsa.pub自动添加到authorized_keys尾部.

**文件名必须是id_rsa.pub**

###attention
.ssh文件夹权限必须是700

.ssh/authorized_keys必须是600

无效时清理下主机.ssh/known_hosts　