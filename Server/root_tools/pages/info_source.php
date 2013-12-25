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
		<h3>项目源码</h3>
        <div class="alert alert-success">
        <a href="https://github.com/rarnu/root-tools">位于 Github 的源码<br/>
        <a href="https://github.com/rarnu/root-tools/fork">Fork 这份源码</a>
        </div>
        <h3>如何编译和发布</h3>
        <font size="3">
        <div class="alert alert-success">
            1. 下载包含了隐藏API的Android4.2 SDK<br>
            <div class="alert alert-warning">
            $ wget wget http://rarnu.7thgen.info/downloads/jellybean17.jar
            </div>
            &nbsp;&nbsp;&nbsp;&nbsp;关于如何自行制作带隐藏API的SDK，请参考<a href="https://github.com/rarnu/root-tools/tree/master/tools">此处</a><br><br>
            2. 备份原始的android.jar<br>
            <div class="alert alert-warning">
            $ mv $(ANDROID_SDK)/platforms/android-17/android.jar ./backup/
            </div>
            3. 用下载的jar包替换原文件，并作重命名<br>
            <div class="alert alert-warning">
            $ mv jellybean17.jar android.jar<br>
            $ cp android.jar $(ANDROID_SDK)/platforms/android-17/
            </div>
            4. 建立以下项目，注意建立项目的顺序<br>
            <div class="alert alert-warning">
            $ android update project -n CommandLib -p . -t android-17<br>
            $ android update project -n CommonUtils -p . -t android-17<br>
            $ android update project -n CommonDevLib -p . -t android-17<br>
            $ android update project -n Emulator -p . -t android-17<br>
            $ android update project -n PackageParser4 -p . -t android-17<br>
            $ android update project -n RootTools -p . -t android-17
            </div>
            5. 编译NDK模块<br>
            <div class="alert alert-warning">
            $ cd Emulator<br>
            $ ndk-build clean &amp;&amp; ndk-build
            </div>
            6. 编译掌优客户端<br>
            <div class="alert alert-warning">
            $ cd $(PROJECT)/RootTools<br>
            $ ant debug
            </div>
            7. 对掌优客户端进行签名<br>
            <div class="alert alert-warning">
            $ cd $(PROJECT)/sign<br>
            $ java -jar signapk.jar rarnu.x509.pem rarnu.pk8 RootTools-debug.apk RootTools_signed.apk
            </div>
            8. 安装并启动掌优<br>
            <div class="alert alert-warning">
            $ adb install -r RootTools_signed.apk<br>
            $ adb shell am start -n com.rarnu.tools.root/.SplashActivity
            </div>
        </div>
        </font>
    </div>
 </div>

</body>

</html>
<?php
include "bottombar.php";
?>
