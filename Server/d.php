<?php
include('navbar.php');
include "database/database.php";
?>
<html>

<head>
<meta charset="UTF-8" />
<meta name="apple-touch-fullscreen" content="yes" />
<meta name="apple-mobile-web-app-capable" content="yes"/>
<meta name="viewport" content="width=device-width, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no"/>
<meta name="format-detection" content="telephone=no"/>

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

<link href="common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">

</head>

<body>

<?php
        $db=openConnection();
        $sql="select * from root_tools_update where tag=0 order by id desc limit 0,5";
        $result=query($db, $sql);
        closeConnection($db);
        $idx=0;
	$lastApkName = "";
	$updateLog = "";
        while (list($id,$name,$ver_code,$size,$filename,$update_desc)=mysql_fetch_row($result)) {
            if ($idx == 0) {
                $updateLog = $updateLog."<div class=\"alert alert-success\">";
		$lastApkName = $filename;
            } else {
                $updateLog = $updateLog."<div class=\"alert alert-warning\">";
            }
            $updateLog = $updateLog."<font size=\"3\">";
            $updateLog = $updateLog."<b>".$name." 更新日志</b><br><div style=\"margin-left:24px\">";
            $updateLog = $updateLog."$update_desc";
            $updateLog = $updateLog."<br></div></font></div>";
	    $idx++;
        }

?>

<?php
	echo "<h4>掌优客户端(<a href=\"root_tools/download/$lastApkName\">点击下载最新版本</a>)</h4>";
?>
	
        <div class="alert alert-warning">
        <font color="red">
        如果您在微信中不能直接通过点击来下载,请点击右上角的"分享"按钮,并选择"在浏览器中打开"
        </font>
        </div>
	
<?php
	echo $updateLog;
?>
        
</body>

</html>

