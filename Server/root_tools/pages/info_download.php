<?php
include('navbar.php');
include "../../database/database.php";
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
		<h3>下载掌优客户端</h3>

        <?php
        $db=openConnection();
        $sql="select * from root_tools_update order by id desc limit 0, 10";
        $result=query($db, $sql);
        closeConnection($db);
        $idx=0;
        while (list($id,$name,$ver_code,$size,$filename,$update_desc)=mysql_fetch_row($result)) {
            if ($idx==0) {
                echo "<div class=\"alert alert-success\">";
            } else {
                echo "<div class=\"alert alert-warning\">";
            }
            echo "<a href=\"../download/$filename\">$name</a><br>";
            echo "<font size=\"3\">";
            echo "<b>更新日志</b><br><div style=\"margin-left:48px\">";
            echo "$update_desc";
            echo "<br></div></font></div>";
            $idx=$idx+1;
        }

        ?>

    </div>
 </div>

</body>

</html>

<?php
include "bottombar.php";
?>
