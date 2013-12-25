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

        <h3>Report</h3>

        <p>
        <div class="alert alert-success">
        <font size="3">
        User Count: 
        <?php
        $db=openConnection();
        $sql="select count(*) c from (select distinct device,osver,email from sbbs_log) a";
        $result=query($db, $sql);
        while (list($count)=mysql_fetch_row($result)) {
            echo $count;
            break;
        }
        ?>
        </font>
        </div>
        <div class="alert alert-warning">
        <font size="3">User email list<br>
        <?php
        $sql="select distinct email from sbbs_log";
        $result=query($db, $sql);
        while (list($email)=mysql_fetch_row($result)) {
            echo "&nbsp;&nbsp;&nbsp;&nbsp;$email<br>";
        }
        ?>
        </font>
        </div>
        <div class="alert alert-error">
        <font size="3">Crash log&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(<a href="#">View All</a>)<br>
        <table border="0" cellspacing="8px" cellpadding="8px">
        <tr><td width="40%"><b>AppVer / OsVer / Email</b></td><td width="50%"><b>Detail</b></td><td width="10%"><b>More</b></td></tr>
        <?php
        $sql = "select * from sbbs_log where action='crash' order by id desc limit 0,50";
        $result=query($db, $sql);
        while (list($id,$device,$appver,$osver,$email,$action,$detail,$actiontime)=mysql_fetch_row($result)) {
            if ($device=="") {
                $device="unknown";
            }
            $str = "<tr><td>$appver / $osver / $email</td><td>$detail</td><td><a href='morelog.php?email=$email&appver=$appver&osver=$osver'>More</a></td></tr>";
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

