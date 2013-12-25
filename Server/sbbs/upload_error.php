<?php
include('navbar.php');
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
</style>

<link href="../common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">

</head>

<body>
 <div class="container">
      <div class="hero-unit">
       
        <div class="alert alert-error"><?php if ($mode==0) { echo "Add Daily Build Error"; } else { echo "Add RC Build Error"; } ?></div>
        <div>
        You must fill the version, version name and choose an apk file for uploading.
        </div><br>
        <p>
        <?php
        $page="dailybuild.php";
        if ($mode!=0) {
            $page="rcbuild.php";
        }
        ?>
          <a class="btn btn-large btn-danger" href="<?php echo $page ?>">Back</a>
        </p>
             
      </div>
 </div>
</body>

</html>
