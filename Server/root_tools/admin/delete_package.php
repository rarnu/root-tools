<?php

include "../../database/database.php";

// delete_package.php?id={1}

$id=$_GET["id"];
$ret = DoDeletePackage($id);

if ($ret == "1") {
	header("Location:manage_package.php");
} else {
	header("Location:delete_fail.php");
}

function DoDeletePackage($i) {
	$sql = "delete from root_tools_recommand where id=$i";
	$db = openConnection();
	$result = query($db, $sql);
	closeConnection($db);
	return $result;
}

?>
