title: 使用github管理hexo本地文件
date: 2014-06-28 11:43:57
tags: [github,hexo]
categories: hexo
---

# 用github管理hexo本地文件夹实现两台电脑同步
---
> 由于hexo+github搭建博客很依赖hexo本地文件,如果家里和公司的hexo本地文件夹不同那么生成的博客样子就不同,每次deploy的时候就有可能会覆盖.加入公司的_post里面有4篇日志,家里的里面有3篇,那么在家里`hexo generate`后在`hexo deploy`就有可能将4篇覆盖成3篇,所以需要公司和家里电脑的hexo本地文件夹同步,我采用github托管.

<!--more-->
1. 首先需要安装git,当然既然已经用hexo写blog了那就肯定安装了git,npm,python,nodejs等了.
2. 然后要有github账户,当然和上面一样,既然在github上写blog肯定也已经有了.(废话)
3. 我们新建一个repository,我命名为hexoblog吧.
4. 假设本地hexo文件夹为hexoblog,我们首先需要用git初始化管理这个目录.
`git init`
这时hexoblog下有个.git隐藏文件夹可以用 ls -al查看出,这是git管理这个文件夹的所有用到的文件.
5.init后git只不过建立了.git还未对任何文件进行管理,我们需要用
`git add .`
"."代表所有文件,我们把hexoblog所有文件加入到git的管理中,这时我们用`git status`可以看到一大片绿的文件名,表明我们已经添加了这些文件的管理.
6. 我们在运行
 `git commit -m "first commit"`提交这些文件的修改.这时我们本的hexoblog管理就完成了,但是还没有提交的github上.
7. 我们在运行
`git remote add origin https://github.com/你的用户名/你的repository名字.git`
指定把本地的文件托管到这个地方.加入我托管的地址是 
>https://github.com/youthy/hexoblog (就是浏览器上面的地址)

那么我的命令就是
`git remote add origin https://github.com/youthy/hexoblog.git`
8. 我们这次制定了托管的地址,下面就可以提交了
`git push origin master`
origin就是我们制定的地址,类似于c++中 define定义的名字一样,如果制定的是`git remote add another http***.git`
那么就可以用another代替origin. master是分支名字,如果你建立了其他分支可以更换这个名字.
这时我们回到github上可以看到提交成功了.
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_004.png)
9. 之后在家里的电脑上执行
`git clone https://你的托管的地址.git`
就可以将文件复制到本地了

***

###实际上还有点小问题.
我们进入themes文件夹下会看到
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_005.png)
主题中有几个没有提交成功.只有landscape提交成功了,对比他们我发现失败的原因是其他的theme本身就有git在负责管理,比如我们在light下执行ls -al 可以看到.git文件夹,但是landscape中没有,这是没有提交成功的原因.
我用`rm -rf .git`
移除了这几个文件夹的git管理文件,然后回到hexoblog下同样移除.git文件.再按上述步骤重新生成提交了一遍就ok了
![](http://youthy-picture.qiniudn.com/%E9%80%89%E5%8C%BA_007.png)
这下终于可以再公司和家两端同步写blog了.

> ##另外发现用git管理hexo后每次进行deploy提交时不用输入用户名和密码了,以前每次deploy都需要输入验证信息.顺便解决了这个反复验证的烦人问题.
