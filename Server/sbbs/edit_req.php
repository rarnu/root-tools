<?php
include "navbar.php";
include "../database/database.php";

$mode=$_GET["mode"];
$id=$_GET["id"];

$name="";
$body="";
$elevel=2;
$version="";

if ($mode==1) {
    $db=openConnection();
    $sql = "select * from sbbs_require where id=$id";
    $result=query($db, $sql);
    while (list($i,$n,$b,$e,$s,$v)=mysql_fetch_row($result)) {
        $name=$n;
        $body=$b;
        $elevel=$e;
        $version=$v;
    }
    closeConnection($db);
}
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

  .ta{
    width:100%;
    overflow:auto;
    }

</style>

<link href="../common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">


</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

                <h3><?php if ($mode==0) { echo "Add Requirement"; } else { echo "Edit Requirement"; } ?> </h3>
                <p>
                <div class="alert alert-success">
                <font size="3">
                <form method="POST" action="add_require.php" class="form-horizontal" enctype="multipart/form-data">
                <input type="hidden" name="mode" value="<?php echo $mode ?>" />
                <input type="hidden" name="id" value="<?php echo $id ?>" />
                <table border="0">
                    <tr>
                        <td>Name</td><td width="80%"><input type="text" name="name" size="40" class="input-block-level" value="<?php echo $name ?>" /></td><td><font color="red">must be filled</font></td>
                    </tr>
                     <tr>
                        <td>Body</td><td width="80%"><textarea rows="10" name="body" class="ta"><?php echo $body ?></textarea></td><td><font color="red">must be filled</font></td>
                    </tr>
                     <tr>
                        <td>Level</td><td><input type="radio" name="elevel" value="1"i <?php if ($elevel==1) { echo "checked"; } ?> />High&nbsp;&nbsp;<input type="radio" name="elevel" value="2" <?php if ($elevel==2){echo "checked";}?> />Normal&nbsp;&nbsp;<input type="radio" name="elevel" value="3" <?php if ($elevel==3){echo "checked";}?> />Low</td><td></td>
                    </tr>
                    <tr>
                        <td>Version</td><td><input type="text" name="version" size="40" class="input-block-level" value="<?php echo $version ?>" /></td><td><font color="red">must be filled</font></td>
                    </tr>
                    <tr height="50px">
                        <td></td><td><input type="submit" value="Submit" class="btn btn-primary" />&nbsp;&nbsp;<input type="reset" value="Reset" class="btn btn-reset" /></td><td></td>
                    </tr>

                </table>
                </form>
                </font>
                </div>
                </p>
      </div>
 </div>
</body>

</html>

