<?php
include "navbar.php";
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

<link href="../common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">


</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

                <h1>Add New Recommand</h1><br><br>

                <form method="POST" action="add_recommand.php" class="form-horizontal" enctype="multipart/form-data">
                <table border="0" >
			<tr>
				<td>Name</td><td><input type="text" class="input-block-level" size="20" name="name" ></td><td></td>
			</tr>
			<tr>
				<td>Jump Mode</td><td><input type="text" class="input-block-level" size="20" name="jump_mode"></td><td>(fill 0 for url jump and 1 for text dialog)</td>
			</tr>
			<tr>
				<td>URL</td><td><input type="text" class="input-block-level" size="20" name="jump_url"></td><td>(take effect only on JUMP_MODE=0)</td>
			</tr>
			<tr>
				<td>TEXT</td><td><input type="text" class="input-block-level" size="20" name="jump_text" ></td><td>(take effect only on JUMP_MODE=1)</td>
			</tr>
			<tr>
				<td>Image File</td><td><input type="file" name="image" class="btn" size="20" ></td><td></td>
			</tr>
			<tr>
				<td>QR File</td><td><input type="file" name="qr" class="btn" size="20" ></td><td></td>
			</tr>
			<tr><td height="20px"></td></tr>
			<tr>
				<td></td><td><input type="submit" value="Submit" class="btn btn-big btn-primary" >&nbsp;&nbsp;<input type="reset" value="Reset" class="btn btn-big btn-primary"></td><td></td>
			</tr>
		</table>
                </form>
      </div>
 </div>
</body>

</html>

