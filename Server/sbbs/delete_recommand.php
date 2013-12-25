<?php

include "../database/database.php";

// delete _recommand.php?id={1}

$id = $_GET["id"];

$ret = doDeleteRecommand($id);
if ($ret != 0) {
	header("Location:page_delete.php");
} else {
	header("Location:delete_fail.php");
}

function doDeleteRecommand($i) {
	$sql = "delete from yugioh_recommand where id=$i";
	$db = openConnection();
	$result = query($db, $sql);
	closeConnection($db);
	return $result;
}

?>
