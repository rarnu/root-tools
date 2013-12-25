<?php
include "navadmin.php";
include "../database/database.php";

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

<script language="javascript">
function changeOrder(id, app_order) {
	location.href="change_order.php?id="+id+"&app_order="+app_order;
}
</script>

</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

		<h1>功能需求管理</h1><br><br>


      </div>

 </div>





</body>

</html>
