<?php

include "database.php";

// change_order.php?id={1}&app_order={2}
$id=$_GET["id"];
$app_order=$_GET["app_order"];

$ret = DoChangeOrder($id, $app_order);
if ($ret == "1") {
	header("Location:order_package.php");
} else {
	header("Location:change_fail.php");
}

function DoChangeOrder($i, $a) {
	$sql = "update root_tools_recommand set app_order=$a where id=$i";
	$db = openConnection();
	$result = query($db, $sql);
	closeConnection($db);

	return $result;	
}

?>
