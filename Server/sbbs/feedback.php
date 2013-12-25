<?php
include "navbar.php";
include "../database/database.php";

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

        <h3>Feedback</h3>

        <p>
        <div class="alert alert-success">
        <font size="3">
        <table border="0" cellspacing="8px" cellpadding="8px">
        <tr><td><b>Device</b></td><td><b>AppVer</b></td><td><b>OsVer</b></td><td><b>Email</b></td><td><b>UserId</b></td><td><b>Feedback</b></td></tr>
        <?php
        $db=openConnection();
        $sql="select * from sbbs_feedback order by id desc limit 0,50";
        $result=query($db, $sql);
        while (list($id,$device,$appver,$osver,$email,$userid,$feedback)=mysql_fetch_row($result)) {
            if ($device=="") {
                $device = "unknown";
            }
            $str = "<tr><td>$device</td><td>$appver</td><td>$osver</td><td>$email</td><td>$userid</td><td>$feedback</td></tr>";
            echo $str;
        }
        closeConnection($db);
        ?>
        </table>
        </font>
        </div>
		</p>

      </div>
 </div>
</body>

</html>

