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

                <h3>Requirement&nbsp;&nbsp;&nbsp;&nbsp;<input type="button" class="btn btn-primary" value="Add New" onclick="location='edit_req.php?mode=0&id=0'"></h3>
                <p>
                <div class="alert alert-success">
                <font size="3">
                <table border="0">
                <tr><td><b>ID</b></td><td width="10%"><b>Name</b></td><td width="5%"><b>Ver</b></td><td width="30%"><b>Body</b></td><td width="10%"><b>Status</b></td><td width="35%"><b>Status Operation</b></td><td><b>Edit</b></td></tr>
                <?php
                $db=openConnection();
                $sql = "select * from sbbs_require order by stat, id desc";
                $result=query($db, $sql);
                while (list($id,$name,$body,$elevel,$stat,$version)=mysql_fetch_row($result)) {
                    $str_stat = "";
                    if ($stat==1) { $str_stat="MoreInfo"; }
                    else if ($stat==2) { $str_stat="Opened"; }
                    else if ($stat==3) { $str_stat="Listed"; }
                    else if ($stat==4) { $str_stat="Rejected"; }
                    else if ($stat==5) { $str_stat="Closed"; }
                    $bgcolor="";
                    if ($elevel==1) { $bgcolor="bgcolor='#FFEBCD'";}
                    else if ($elevel==2) {$bgcolor="";}
                    else if ($elevel==3) {$bgcolor="bgcolor='#E6E6FA'";}
                    if ($stat==5) {$bgcolor="bgcolor='#F5F5F5'";}
                    $str="<tr style='border-bottom:1px solid #99cc00;' $bgcolor><td>$id</td><td>$name</td><td>$version</td><td>$body</td><td>$str_stat</td><td><a href='change_stat.php?id=$id&stat=2'>Open</a>&nbsp;|&nbsp;<a href='change_stat.php?id=$id&stat=5'>Close</a>&nbsp;|&nbsp;<a href='change_stat.php?id=$id&stat=3'>List</a>&nbsp;|&nbsp;<a href='change_stat.php?id=$id&stat=4'>Reject</a>&nbsp;|&nbsp;<a href='change_stat.php?id=$id&stat=1'>MoreInfo</a></td><td><input type='button' class='btn btn-primary' value='Edit' onclick=\"location='edit_req.php?mode=1&id=$id'\"></td></tr>";
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

