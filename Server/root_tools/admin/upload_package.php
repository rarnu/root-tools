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

		<h1>上传推广软件</h1><br><br>
        <div class="alert alert-success">
        本页面不再生效, 请使用上传脚本
		<table border="0" >
			<tr>
				<td>名称</td> <td><input type="text" name="name" size="20" class="input-block-level" disabled="disabled" ></td>
			</tr>
			<tr>
				<td>包名</td><td><input type="text" name="package_name" size="20" class="input-block-level" disabled="disabled"></td>
			</tr>
			<tr>
				<td>Unix 名称</td><td><input type="text" name="unix_name" size="20" class="input-block-level" disabled="disabled"></td>
			</tr>
			<tr>
				<td>图标</td><td><input type="file" name="icon" size="20" class="btn" disabled="disabled" ></td>
			
			</tr>
			<tr>
				<td>服务端文件名</td><td><input type="text" name="apk" size="20" class="input-block-level" disabled="disabled"></td>
			</tr>
			<tr>
				<td></td><td height="50">&nbsp;&nbsp;&nbsp;&nbsp;<input type="submit" value="提交" class="btn btn-big btn-primary" disabled="disabled">&nbsp;&nbsp;<input type="button" value="重置" class="btn btn-big btn-primary" disabled="disabled"></td>
			</tr>
		
		</table>
		
        </div>


      </div>

 </div>





</body>

</html>
