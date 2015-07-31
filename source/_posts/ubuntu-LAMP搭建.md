title: ubuntu下lamp(apache2+php5+mysql)搭建
date: 2015-07-31 13:20:15
tags: ubuntu

---

ubuntu下lamp(apache2+php5+mysql)搭建
<!--more-->

<!-- toc -->

## 安装apache2  
```
sudo apt-get install apache2
```

此时浏览器进入localhost会显示It works界面。

## 安装php
```
sudo apt-get install php5  
sudo apt-get install libapache2-mod-php5  
sudo /etc/init.d/apache2 restart 
```

```
sudo vim /var/www/html/info.php
```

输入下面的内容：
```
<?php
phpinfo();
?>
```

然后打开浏览器访问 (http://127.0.0.1/info.php):
你可以看到一些已经支持的模块。

> 如果在www下没有html文件夹，info就建在www下。

## 安装mysql
```
sudo apt-get install mysql-server  
```

输入root用户密码。

## 让apache、php支持mysql  
```
sudo apt-get install libapache2-mod-auth-mysql  
sudo apt-get install php5-mysql  
sudo apt-get install php5-mcrypt
sudo php5enmod mcrypt
sudo /etc/init.d/apache2 restart
```

可选（apt-get install php5-mysqlnd
sudo apt-get install php5 libapache2-mod-php5 php5-cgi php5-cli php5-common php5-curl php5-gd php5-mysql php5-pgsql）

## 安装phpmyadmin
```
apt-get install phpmyadmin
```

选择apache2.
安装完成后，建立软链。
```
sudo ln -s /usr/share/phpmyadmin /var/www/html 
```

此时可以进入localhost/phpmyadmin

## 配置
### php
```
sudo vim /etc/php5/apache2/php.ini
```

找到default_charset改为utf-8
`default_charset = “UTF-8″`


### apache
```
sudo gedit /etc/apache2/apache2.conf
```

结尾添加
`AddDefaultCharset UTF-8`
`ServerName 127.0.0.1`

完成。

## 参考
http://www.2cto.com/os/201211/165190.html
http://www.jb51.net/article/39127.htm
http://forum.ubuntu.org.cn/viewtopic.php?t=251355
