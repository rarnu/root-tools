<?php
include('navbar.php');

?>
<html>

<head>
<meta http-equiv="Content-Language" content="en-us">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<title>RootTools</title>
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

<link href="../../common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="../../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">

</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">
       <h3>全部功能列表(<a href="info_page.php">返回</a>)</h3>
        <div class="alert alert-success">
        <table border="0" cellpadding="16">
        <tr><td width="5%">*</td><td width="95%"><b>系统应用管理</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 删除系统应用<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 将应用将加到自定义清理列表<br>
        &nbsp;&nbsp;&nbsp;&nbsp;c) 添加应用到系统中<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<font color="RED">d) 从服务器端获取指定系统的安全内置应用列表（计划中）</font></td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>可用性管理</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 禁用或启用一个应用程序</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>强制升级</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 扫描SD卡中的安装包<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 检查新老版本应用程序的签名是否一致<br>
        &nbsp;&nbsp;&nbsp;&nbsp;c) 强制对签名不一致的应用程序进行升级操作</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>应用组件管理</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 展现应用程序内的组件列表<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 禁用/启用指定的组件<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<font color="RED">c) 从服务器端获取指定应用程序的安全组件列表（计划中）</font></td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>Root &amp; Busybox</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 展现系统内ROOT必备文件的安装状态<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 为用户安装BUSYBOX</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>清理ROM</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 自动化批量删除系统内置的第三方应用<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<font color="RED">b) 增加对更多品牌机型的支持（计划中）</font></td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>备份应用数据</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 批量备份已安装的应用程序以及其数据<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 备份存在于SD卡中的应用数据</td></tr>
        <tr><td width="5*">*</td><td width="95%"><b>还原应用数据</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 批量还原已备份的应用程序以及其数据<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 还原应存在于SD卡中的应用数据<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<font color="RED">c) 支持特定系统中需要在安装前进行扫描的情况（计划中）</font></td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>清理内存</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 瞬间清理整机内存<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 杀掉单独进程并释放该进程所占的内存<br>
        &nbsp;&nbsp;&nbsp;&nbsp;c) 将应用程序添加到忽略列表中以避免被清理</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>清理缓存文件</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 展现系统中的缓存文件情况<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 清理单个应用程序的缓存文件<br>
        &nbsp;&nbsp;&nbsp;&nbsp;c) 清理整个系统中的缓存文件</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>清理Dalvik</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 清理Dalvik虚拟机内的缓存、遗留文件</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>磁盘信息</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 展现系统中所有分区的使用信息<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 对于使用量过高的分区进行提醒</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>文件系统</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 展现系统内的目录及文件<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 常规文件操作，剪切，复制，粘贴等<br>
        &nbsp;&nbsp;&nbsp;&nbsp;c) 文本文件编辑<br>
        &nbsp;&nbsp;&nbsp;&nbsp;d) APK 文件的安装，可提示异常信息<br>
        &nbsp;&nbsp;&nbsp;&nbsp;e) APK 文件的强制安装，系统存储空间不够时也可安装</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>修改Hosts</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 展现系统内当前的Hosts列表<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 搜索并添加指定域名的Hosts<br>
        &nbsp;&nbsp;&nbsp;&nbsp;c) 删除指定的Hosts<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<font color="RED">d) 自动搜索并添加Hosts（计划中）</font></td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>扫描媒体文件</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 自动扫描系统媒体文件的改动，如音乐，照片等</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>检查网络状况</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 检查联网状态，网络测速等</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>重启手机</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 强制重启手机</td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>终端模拟器</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 给予用户一个自行玩转手机内一切的命令平台<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<font color="RED">b) 方便终端输入的输入法（计划中）</font></td></tr>
        <tr><td width="5%">*</td><td width="95%"><b>设置</b><br>
        &nbsp;&nbsp;&nbsp;&nbsp;a) 桌面小浮窗设置<br>
        &nbsp;&nbsp;&nbsp;&nbsp;b) 系统应用管理设置<br>
        &nbsp;&nbsp;&nbsp;&nbsp;c) 备份还原设置<br>
        &nbsp;&nbsp;&nbsp;&nbsp;d) 清理内存设置<br>
        &nbsp;&nbsp;&nbsp;&nbsp;e) 修改Hosts设置</td></tr>
        </table>
      </div>
 </div>

</body>

</html>

<?php
include "bottombar.php";
?>
