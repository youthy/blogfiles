title: github屏蔽百度爬虫的解决办法
date: 2015-08-04 14:57:55
tags: [hexo, github]
---

google上可以搜到github博客的内容，但是百度是搜不到的。用百度的站长工具抓取也总是抓取失败。是因为github把百度爬虫给屏蔽了，而且将会长期下去。导致百度是无法收录自己的网站。
<!--more-->
[如何解决百度爬虫无法爬取搭建在Github上的个人博客的问题--知乎](http://www.zhihu.com/question/30898326)
google下github + 百度爬虫也有很多文章。
有几种方案 
  * 通过CDN加速，弊端是由于缓存的关系，会导致经常出现404的错误
  * 用gitcafe镜像
  * 用vps，让百度从自己的vps爬虫。
 
虽然自己有vps，但是其实目前对我来说操作起来最简单的还是用gitcafe做镜像。我只需要修改下hexo的config文件添加一个deploy的对象就可以将博客部署到gitcafe和github两个地方，然后在dnspod上添加一条域名解析就可以了。

## 创建gitcafe账号并建立一个repo，名字要和用户名一致
![](/img/gitcafe1.png)

默认分支这时候如果没有先不用管。等到之后在hexo的_config里面直接加就好了，之后再回来修改它为gitcafe-pages

![](/img/gitcafe2.png)

在ssh公钥管理里面添加和github一样的内容就好了，我的在~/.ssh/rsa_pub文件里。
然后在
![](/img/gitcafe3.png)
在hexo的_config.yml里面加入gitcafe的地址。并改成如上形式。之后hexo d就可以部署到github和gitcafe上了。

## 到dnspod修改域名解析。
![](/img/gitcafe4.png)

在dnspod里面让百度走gitcafe，其他人还是走github。 也可以在细化让国内也走gitcafe，国外走github。

之后用百度站长工具看到百度可以成功抓取了
![](/img/gitcafe5.png)

（无论在github还是gitcafe移动UA都是可以成功抓取的）。

## 关于404 not found

要确保
 * gitcafe的项目名一定要和用户名一致，并且branch是gitcafe-pages，且为默认branch
 * 如果绑定了域名，一定要在设置里加入域名，比如我的
![](/img/gitcafe6.png)

以上任意一项不足都可能导致404 not found。
