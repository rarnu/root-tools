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
                <h3>用户反馈</h3>
        <table border="1" width="90%">
        <tr><td width="10%">AppVer</td><td width="10%">OsVer</td><td width="10%">Device</td><td width="10%">Email</td><td>Feedback</td></tr>
        <?php
        $db = openConnection();
        $sql = "select * from yugioh_feedback order by id desc limit 0, 100";
        $result = query($db, $sql);
        closeConnection($db);
        while (list($id,$device,$email,$feedback,$appver,$osver)=mysql_fetch_row($result)) {
            $str = "<tr><td>$appver</td><td>$osver</td><td>$device</td><td>$email</td><td>$feedback</td></tr>";
            echo $str;
        }
        ?>
        </table>
    </div>
</div>
</body>

</html>

