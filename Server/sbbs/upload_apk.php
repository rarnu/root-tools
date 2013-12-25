<?php
include "navbar.php";
include "../database/database.php";

// mode=0: daily
// mode=1: rc
$mode=$_GET["mode"];

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

                <h3><?php if ($mode==0) { echo "Add Daily Build"; } else { echo "Add RC Build"; } ?> </h3>
                <p>
                <div class="alert alert-success">
                <font size="3">
                <form method="POST" action="upload_package.php" class="form-horizontal" enctype="multipart/form-data">
                <input type="hidden" name="mode" value="<?php echo $mode ?>" />
                <table border="0">
                    <tr>
                        <td>VersionCode</td><td width="80%"><input type="text" name="version" size="40" class="input-block-level" /></td><td><font color="red">must be filled</font></td>
                    </tr>
                     <tr>
                        <td>VersionName</td><td width="80%"><input type="text" name="version_name" size="40" class="input-block-level" /></td><td><font color="red">must be filled</font></td>
                    </tr>
                    <?php if ($mode==0) {
                        ?>
                    <tr>
                        <td>UpdateLog</td><td width="80%"><textarea rows="10" name="update_log" class="ta"></textarea></td><td></td>
                    </tr>
                    <?php }
                    ?>
                    <tr>
                        <td>Size</td><td><input type="text" name="size" size="40" class="input-block-level" /></td><td><font color="red">must be filled</font></td>
                    </tr>
                    <tr>
                        <td>APK File</td><td width="80%"><input type="file" name="file" size="40" class="input-block-level"></td><td><font color="red">must upload an apk file</font></td>
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

