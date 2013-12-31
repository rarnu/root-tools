<?php
include "navadmin.php";
include "../../database/database.php";

function DoGenerateTable() {
	$sql = "select id, device, user_memo from root_tools_update_user order by id asc";
	$db = openConnection();
	$result = query($db, $sql);
	closeConnection($db);
	$str = "";
	while (list($id, $device, $user_memo)=mysql_fetch_row($result)) {
		$str = $str."<tr height='40'><td valign='top'>$id</td><td valign='top'>$device</td><td valign='top'>$user_memo</td><td valign='top'><input type='button' value='删除' class='btn btn-small btn-primary' onClick=\"deleteDevice($id);\"></td><tr>";
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
var id_del = 0;
function deleteDevice(id) {
    id_del = id;
	$('#confirmDialog').modal('show');
}
function doDelete() {
	location.href="delete_device.php?id="+id_del;
}
</script>

</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

		<h1>管理内测用户</h1><br><br>
        <div class="alert alert-success">

        <form method="POST" action="add_device.php" class="form-horizontal" enctype="multipart/form-data">
        <table border="0" width="50%">
        <tr><td>设备号</td><td><input type="text" name="device" size="20" class="input-block-level"></td></tr>
        <tr><td>用户名</td><td><input type="text" name="username" size="10" class="input-block-level"></td></tr>
        <tr><td></td><td><input type="submit" value="增加" class="btn btn-big btn-primary">&nbsp;&nbsp;<input type="reset" value="重置" class="btn btn-big btn-primary"></td></tr>
        </table>
        </form>

		<table border="0" width="80%">
		<tr>
			<td width="10%"><b>序号</b></td><td width="50%"><b>设备号</b></td><td><b>备注</b></td><td>&nbsp;</td>
		</tr>
		<tr><td>&nbsp;</td></tr>
		<?php
		echo DoGenerateTable();
		?>

		</table>
        </div>
      </div>

 </div>

<section id="modals">
<div id="confirmDialog" class="modal hide">
<div class="modal-header">
	<a class="close" data-dismiss="modal">&times;</a>
	<h3>删除</h3>
</div>
<div class="modal-body">
	<h4>确定要删除选中的软件？</h4>
</div>
<div class="modal-footer">
	<a href="#" class="btn" data-dismiss="modal">取消</a>
	<a onClick="doDelete();" class="btn btn-primary" data-dismiss="modal">确定</a>
</div>
</div>
</section>


</body>

</html>
