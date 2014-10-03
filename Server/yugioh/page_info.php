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

<script type="text/javascript">
    function showUpdateLog() {
        $("#_prop_log").modal("toggle");
    }
    function showOpensourceInfo() {
        $("#_prop_opensource").modal("toggle");
    }
</script>

</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">
        <h3>游戏王卡片查询器 下载</h3><br>
        <table border="0" width="60%" cellpadding="8">
            <tr><td width="30%">Android (Ver 3.0.6)</td><td width="30%">iOS (Ver 3.0.6)</td></tr>
            <tr><td width="30%"><a href="download/YuGiOhCard.apk">点击下载APK</a></td><td width="30%"><a href="download/YuGiOhCard.ipa">点击下载IPA(越狱版)</a><br>AppStore版(审核中)</td></tr>
            <tr><td width="30%"><img src="image/qr_download.png" width="120" height="120"></td><td width="30%"><img src="image/qr_ios.png" width="120" height="120"></td></tr>
        </table>
		<br>
        <h3>软件特性
            &nbsp;<input type="button" value="查看更新历史" class="btn btn-primary" onclick="showUpdateLog();">
            &nbsp;<input type="button" value="查看开源信息" class="btn btn-primary" onclick="showOpensourceInfo();">
        </h3>
		<p>
1. 超快速搜索，瞬间搜索得到结果毫不费力<br>
2. 任意配置的手机均可流畅使用<br>
3. 自动更新，获取新版本，获取新卡片数据及卡图不再费力<br>
4. 内置调整数据，决斗时不再口糊效果<br>
5. 决斗小工具，硬币骰子一样不差<br>
6. 全新的抽屉式操作界面给你不一样的感受<br>
		</p><br>

		<h3>软件截图</h3><br>
          <table border="0" cellpadding="1" width="100%">
              <tr><td width="30%"><img src="image/p1.png"></td><td width="30%"><img src="image/p2.png"></td><td width="30%"><img src="image/p3.png"></td></tr>
              <tr><td width="30%"><img src="image/p4.png"></td><td width="30%"><img src="image/p5.png"></td><td width="30%"><img src="image/p6.png"></td></tr>
              <tr><td width="30%"><img src="image/p7.png"></td><td width="30%"><img src="image/p8.png"></td><td width="30%"><img src="image/p9.png"></td></tr>
          </table>
      </div>
 </div>

 <?php
 include "popup/popup_log.php";
 include "popup/popup_opensource.php";
 ?>

</body>

</html>

