<?php
include "navbar.php";
include "../database/database.php";

function DoGenerateTable() {
	$sql = "select id, name, jump_mode, jump_url, jump_text from yugioh_recommand order by id asc";
	$db = openConnection();
	$result = query($db, $sql);
	closeConnection($db);
	$str = "";
	while (list($id, $name, $jump_mode, $jump_url, $jump_text)=mysql_fetch_row($result)) {
		$str = $str."<tr height='40'><td valign='top'>$id</td><td valign='top'>$name</td><td valign='top'>$jump_mode</td><td valign='top'>$jump_url</td><td valign='top'>$jump_text</td><td valign='top'><input type='button' value='Delete' class='btn btn-small btn-primary' onClick=\"deleteRecommand($id);\"></td><tr>";
	}
	return $str;
}
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
var id_del = 0;
function deleteRecommand(id) {
	id_del = id;
	$('#confirmDialog').modal('show');
}
function doDelete() {
	location.href="delete_recommand.php?id="+id_del;
}
</script>

</head>

<body>
 <div class="container">
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">

		<h1>Manage Recommand</h1><br><br>

		<table border="0" width="80%">
		<tr>
			<td width="10%"><b>ID</b></td><td width="25%"><b>NAME</b></td><td><b>JUMP MOE</b></td><td><b>JUMP URL</b></td><td><b>JUMP TEXT</b></td><td>&nbsp;</td>
		</tr>
		<tr><td>&nbsp;</td></tr>
		<?php
		echo DoGenerateTable();
		?>

		</table>

      </div>

 </div>

<section id="modals">
<div id="confirmDialog" class="modal hide">
<div class="modal-header">
	<a class="close" data-dismiss="modal">&times;</a>
	<h3>Delete</h3>
</div>
<div class="modal-body">
	<h4>Are you sure to delete selected recommand?</h4>
</div>
<div class="modal-footer">
	<a href="#" class="btn" data-dismiss="modal">Cancel</a>
	<a onClick="doDelete();" class="btn btn-primary" data-dismiss="modal">OK</a>
</div>
</div>
</section>


</body>

</html>
