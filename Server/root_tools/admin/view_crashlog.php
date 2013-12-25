<?php
include "navadmin.php";
include "../../database/database.php";

function generateCrashTable() {
    $db = openConnection();
    $sql = "select deviceId,app_version,os_version,module,mail,crash, crash_time from root_tools_crash where mail != '' order by id desc limit 0, 500";
    $result = query($db, $sql);
    while (list($d,$av,$ov,$m,$mail,$c,$ct)=mysql_fetch_row($result)) {
        echo "<tr><td>$ct</td><td>$av</td><td>$ov</td><td>$m</td><td>$mail</td><td style='word-break:break-all'>$c</td></tr>";
    }
    closeConnection();
}

?>

<html>

<head>
<meta http-equiv="Content-Language" content="en-us">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<title>RootTools Admin</title>
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

		<h1>查看崩溃日志</h1><br><br>
        <div class="alert alert-success">
        <table border="0" cellspacing="8" cellpadding="8">
        <tr><td width="10%"><b>时间</b></td><td width="10%"><b>应用版本</b></td><td width="10%"><b>系统版本</b></td><td width="10%"><b>设备型号</b></td><td width="20%"><b>邮件地址</b></td><td><b>反馈信息</b></td></tr>    
        <?php
            generateCrashTable();
            ?>
        </table>
        </div>
      </div>

 </div>





</body>

</html>
