<?php
include('navbar.php');

?>
<html>

<head>
<meta http-equiv="Content-Language" content="en-us">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<title>RootTools Recommand</title>
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

<link href="common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">

</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

		<h1>Upload Recommand Packages</h1><br><br>

		<form method="POST" action="upload.php" class="form-horizontal" enctype="multipart/form-data">
		<table border="0" >
			<tr>
				<td>Name</td> <td><input type="textbox" name="name" size="20"></td>
			</tr>
			<tr>
				<td>Package Name</td><td><input type="textbox" name="package_name" size="20"></td>
			</tr>
			<tr>
				<td>Main Activity</td><td><input type="textbox" name="main_activity" size="20"> </td>
			</tr>
			<tr>
				<td>Unix Name</td><td><input type="textbox" name="unix_name" size="20"></td>
			</tr>
			<tr>
				<td>Upload Icon</td><td><input type="file" name="icon" size="20" ></td>
			
			</tr>
			<tr>
				<td>Upload package</td><td><input type="file" name="apk" size="20"></td>
			</tr>
			<tr>
				<td></td><td height="50">&nbsp;&nbsp;&nbsp;&nbsp;<input type="submit" value="Submit" class="btn btn-big btn-primary">&nbsp;&nbsp;<input type="button" value="Reset" class="btn btn-big btn-primary"></td>
			</tr>
		
		</table>
		
		</form>


      </div>

 </div>





</body>

</html>
