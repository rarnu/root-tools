<?php
include('navadmin.php');

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

<link href="../../common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="../../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">

</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

		<h1>上传开机画面</h1><br><br>
        <div class="alert alert-success">
		<form method="POST" action="upload_new_splash.php" class="form-horizontal" enctype="multipart/form-data">
        <table border="0" width="80%" >
			<tr>
				<td>图片尺寸应当是800x600<br>格式应当是PNG或JPG</td>
                <td><input type="file" name="picture" size="20" class="input-block-level" ></td>
			</tr>
			<tr>
				<td></td>
                <td height="50">&nbsp;&nbsp;&nbsp;&nbsp;<input type="submit" value="提交" class="btn btn-big btn-primary" >&nbsp;&nbsp;<input type="reset" value="重置" class="btn btn-big btn-primary"></td>
			</tr>
		
		</table>
        </form>
		
        </div>


      </div>

 </div>





</body>

</html>
