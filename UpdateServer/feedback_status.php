<?php

require_once "database.php";

$id = $_REQUEST["id"];
$stat = $_REQUEST["stat"];

$ret = DoStatus($id, $stat);
echo $ret;

function DoStatus($i, $s) {
	$str = "{\"result\":1}";
	$db = openConnection();

	$stmt = $db->prepare("update feedback set status = ? where id = ?");
	$stmt->bind_param("ii", $s, $i);
	$stmt->execute();
	$rows = $stmt->affected_rows;
	$stmt->close();
	closeConnection($db);
	if ($rows != 0) {
		$str = "{\"result\":0}";
	}
	return $str;
}

?>