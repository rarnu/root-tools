<?php
include "navbar.php";
?>
<html>

<head>
<meta http-equiv="Content-Language" content="en-us">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="description" content>
<meta name="author" content>
<!-- Le styles -->
<style>
      body {
        padding-bottom: 30px;
      }
      .hero-unit {
        margin-top: 20px;
      }
</style>

<link href="../common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">


</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

                <h3>Today's Build&nbsp;&nbsp;&nbsp;&nbsp;<input type="button" class="btn btn-primary" value="Add New" onclick="location='upload_apk.php?mode=0'"/></h3>

                <p>
                <a href="#">Please Wait...</a><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复消息互篡的问题<br>
                    2. 增加未读信息的显示<br>
                    3. 数据库结构变更，本版本需要先卸载老版本后才可安装<br>
                    4. 修复下拉时焦点的问题

                    </font>
                    </div>
		</p>

		<h3>History Builds</h3>
        <p>
                <a href="download/SbbsMeClient_20130904.apk">SbbsMeClient_20130904.apk</a><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复消息反复提醒的问题<br>
                    2. 修复本地缓存无法创建导致整个程序报错的问题<br>
                    3. 跟随API端改动修改客户端API实现<br>
                    4. 增加私信用户列表获取的功能<br>
                    5. 增加私信内容的对话形式呈现<br>
                    6. 增加多语言支持
                    </font>
                    </div>
		</p>
        <p>
                <a href="download/SbbsMeClient_20130823.apk">SbbsMeClient_20130823.apk</a><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复小屏手机上登录入口消失的问题<br>
                    2. 新的返回键逻辑<br>
                    3. 无新内容时,将不再显示"加载更多"按钮<br>
                    4. 修复服务中未登录导致反复崩溃的问题<br>
                    5. 接入新的私信API<br>
                    6. 修复私信显示名字错误的问题<br>
                    7. 增加私信通知<br>
                    8. 修复本地数据库读写异常的问题<br>
                    9. 修复私信反复下载的问题<br>
                    10. 增加视频选择的功能(未嵌入文章内)<br>
                    11. 优化登录流程,提高登录的速度
                    </font>
                    </div>
		</p>
        <p>
                <a href="download/SbbsMeClient_20130813.apk">SbbsMeClient_20130813.apk</a><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复获取帐号时产生的崩溃<br>
                    2. 修复ActionBar内用户头像尺寸过大的问题<br>
                    3. 增加图片上传的进度<br>
                    4. 修改代码中部分命名不规范的情况<br>
                    5. 增加自动升级的接口<br>
                    6. 主界面增加用户私信入口(功能未实现)<br>
                    7. 增加消息服务和监听器<br>
                    8. 增加发送私信的功能<br>
                    9. 增加获取私信,组织用户列表的功能<br>
                    10. 增加用于私信的本地缓存<br>
                    11. 增加成就系统的入口(功能未实现)<br>
                    12. 当前版本大幅修改了本地数据库，若产生崩溃，请删除/sdcard/.sbbsme目录
                    </font>
                    </div>
		</p>

        <p>
                <a href="download/SbbsMeClient_20130802.apk">SbbsMeClient_20130802.apk</a><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 增加更明显的登录状态提示<br>
                    2. 修正获取设备唯一序号时产生的崩溃<br>
                    3. 增加未登录时长按评论的提示<br>
                    4. 修复登出时图标异常的问题<br>
                    5. 增加Block长按菜单的左右滑动切换<br>
                    6. 修复相册进入速度太慢的问题<br>
                    7. 增加提取文章内的图片与链接<br>
                    8. 修复登陆时反复点击登陆按钮的问题<br>
                    9. 查看文章内链接和大图<br>
                    10. 修复查看链接时产生的崩溃
                    </font>
                    </div>
		</p>
        <p>
                <b>20130731</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修改左侧的点为返回上一级<br>
                    2. 修正文章列表本地缓存会直接缓存第二页的问题<br>
                    3. 完成About界面<br>
                    4. 加入完整的日志收集
                    </font>
                    </div>
		</p>
        <p>
                <b>20130730</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复因图片过大导致的崩溃<br>
                    2. 修复因图片下载失败导致的崩溃<br>
                    3. 修复因SD卡空间满导致的图片显示异常<br>
                    4. 增加文章的分页获取显示<br>
                    5. 修正一处无需AutoFetch的错误显示<br>
                    6. 尝试修复Gallery下Decode产生的异常，未测试<br>
                    7. 修复因Fragment构造函数引发的崩溃<br>
                    8. 增加相册图片刷新功能<br>
                    9. 增加文章列表的本地缓存<br>
                    10. 修正部分手机登陆时显示js执行异常的问题<br>
                    11. 修正无网络时看文章导致的崩溃问题<br>
                    12. 增加相册的本地缓存<br>
                    13. 增加标签列表的本地缓存<br>
                    14. 修复因API异常后未做处理导致的无数据问题
                    </font>
                    </div>
		</p>
        <p>
                <b>20130726</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复图文混排引起的一处崩溃<br>
                    2. 增加相册内添加图片的功能<br>
                    3. 增加拍照，选择图片，剪裁图片的功能<br>
                    4. 增加删除相册内照片的功能<br>
                    5. 增加将照片插入到文章内的功能<br>
                    6. 修改发文章界面，格式按钮移到下方<br>
                    7. 修改编辑文章界面，增加插入图片按钮<br>
                    8. 修复主界面因文章太长导致不正常拉伸的问题
                    </font>
                    </div>
		</p>
        <p>
                <b>20130724</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 增加文章显示时的图文混排<br>
                    2. 完成反馈功能<br>
                    3. 完成用户统计功能<br>
                    4. 增加崩溃日志收集的功能<br>
                    </font>
                    </div>
		</p>
        <p>
                <b>20130723</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 增加代码视图的导航条<br>
                    2. 增加用户反馈的API<br>
                    3. 修正看评论时，上一层未回弹的问题<br>
                    4. 修正评论时反复添加comment字样的问题<br>
                    5. 增加完整的MultiPart文件上传支持<br>
                    6. 增加关于页面(未完成)<br>
                    7. 增加反馈页面<br>
                    8. 清理项目
                    </font>
                    </div>
		</p>
        <b>20130719</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复部分文章因无public字段导致的不显示问题<br>
                    2. 增加以彩色方块标识评论数<br>
                    3. 增加分享文章的功能<br>
                    4. 重构部分与github模块相关的代码<br>
                    5. 增加代码刷新逻辑<br>
                    6. 增加本地缓存<br>
                    7. 修改分享页面为统一风格
                    </font>
                    </div>
		</p>
        <p>
                <b>20130716</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 增加无数据时的提示和重新加载<br>
                    2. 修正未登录时，程序显示为已登录的问题<br>
                    3. 修正Block热区滑动不流畅的问题(暂未针对720p以下屏幕优化)<br>
                    4. 增加关注，取消关注的功能<br>
                    5. 修正因窗口关闭导致的无法找到对象的问题<br>
                    6. 在查看用户时，增加查看用户最新文章的功能
                    </font>
                    </div>
		</p>
        <p><b>20130715</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 增加github数据本地缓存(查询部分)<br>
                    2. 评论浏览改为右侧滑模式<br>
                    3. 去除列表边缘发光效果<br>
                    4. 增加左侧Block的显示<br>
                    5. 增加评论数的显示(未完善)<br>
                    6. 清理项目，合并代码
                    </font>
                    </div>
		</p>
        <p>
                <b>20130711</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 增加MemberTreeHistory(GITHUB)<br>
                    2. 增加MemberCodeTreeHistory(GITHUB)<br>
                    3. 修复50次请求限制的问题<br>
                    4. 清理不需要的log<br>
                    5. 增加HotTags展示<br>
                    6. 增加跟据Tag搜索文章并展示<br>
                    7. 修改post new界面的部分UI<br>
                    8. 显示基本的用户信息<br>
                    9. 增加Data Provider<br>
                    10. 增加评论显示，子评论显示<br>
                    11. 评论相关操作，发表子评论(不完善，谨慎操作)<br>
                    12. 增加在评论中追加Block<br>
                    13. 增加修改评论<br>
                    14. 增加删除评论<br>
                    15. 增加文章页与评论页的消息传递，相互刷新机制
                    </font>
                    </div>
		</p>
 <p>
                <b>20130710</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 修复用户登录后，反复加载用户信息的问题<br>
                    2. 增加发帖时的登录状态检查<br>
                    3. 修复Recent模块的信息加载异常<br>
                    4. 修改ActionBar下拉菜单为统一样式<br>
                    5. 增加获取用户信息的API<br>
                    6. 增加获取全部Tag的API<br>
                    7. 增加查看单个Block的界面(暂未展现内容)<br>
                    8. 增加从Recent跳入单个Block(暂未展现内容)
                    </font>
                    </div>
		</p>
		<p>
        <b>20130709</b><br>
                    <div class="alert alert-success">
                    <font size="3">
                    1. 增加发帖功能<br>
                    2. 修复删除Block不刷新的问题<br>
                    3. 增加删除整个帖子的功能<br>
                    4. 修复oauth认证崩溃的问题<br>
                    5. 修复tag不唯一导致找不到Fragment的问题<br>
                    6. 修复主界面偶尔无法刷新的问题<br>
                    7. 增加输入框样式
                    </font>
                    </div>

		</p><br>

      </div>
 </div>
</body>

</html>

