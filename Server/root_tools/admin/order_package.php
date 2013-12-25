<?php
include "navadmin.php";
include "../../database/database.php";

function DoGenerateTable() {
	$sql = "select id, name, app_order from root_tools_recommand order by app_order asc";
	$db = openConnection();
	$result = query($db, $sql);
	closeConnection($db);
	$str = "";
	while (list($id, $name, $app_order)=mysql_fetch_row($result)) {
		$str = $str."<tr height='40'><td valign='top'>$id</td><td valign='top'>$name</td><td valign='top'>$app_order</td><td valign='top'><input type='text' id='t$id' value='$app_order' class='input-block-level'></td><td valign='top'><input type='button' value='更改' class='btn btn-small btn-primary' onClick=\"changeOrder($id, document.getElementById('t$id').value);\"></td><tr>";
	}
	return $str;
}
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

		<h1>更改推广软件排序</h1><br><br>
        <div class="alert alert-success">
		<table border="0" width=80%>
		<tr>
			<td width="10%"><b>序号</b></td><td width="25%"><b>名称</b></td><td width="15%"><b>当前排序</b></td><td width="15%"><b>新的排序</b></td><td>&nbsp;</td>
		</tr>
		<tr><td>&nbsp;</td></tr>
		<?php
		echo DoGenerateTable();
		?>

		</table>
        </div>

      </div>

 </div>





</body>

</html>
