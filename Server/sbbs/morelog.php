<?php
include "navbar.php";
include "../database/database.php";

$email=$_GET["email"];
$appver=$_GET["appver"];
$osver=$_GET["osver"];

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

        <h3>Log Detail&nbsp;&nbsp;&nbsp;&nbsp;(<a href="report.php">Back</a>)</h3>

        <p>
        <div class="alert alert-success">
        <font size="3">
        <?php echo "Email: $email<br>Appver: $appver<br>OsVer: $osver<br><br>"; ?>
        <table border="0">
        <tr><td width="30%"><b>Action</b></td><td width="50%"><b>Detail</b></td><td width="20%"><b>Time</b></td></tr>
        <?php
        $db=openConnection();
        $sql="select * from sbbs_log where email='$email' and appver=$appver and osver='$osver' order by id desc limit 0,100";
        $result=query($db, $sql);
        while (list($id,$device,$appver,$osver,$email,$action,$detail,$actiontime)=mysql_fetch_row($result)) {
            $str="<tr><td>$action</td><td>$detail</td><td>$actiontime</td></tr>";
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

