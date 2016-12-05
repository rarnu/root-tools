<?php

require_once "database.php";

$ret = DoQuery();
echo $ret;

function DoQuery() {
	$str = "{\"result\":0, \"data\":[";
	$db = openConnection();
	$stmt = $db->prepare("select * from thanks order by id asc");
	$stmt->execute();
	$id = 0;
	$name = "";
	$desc = "";
	$head = "";
	$stmt->bind_result($id, $name, $desc, $head);
	while ($stmt->fetch()) {
		$str .= "{\"id\":${id},\"name\":".json_encode($name).",\"desc\":".json_encode($desc).",\"head\":\"${head}\"},";
	}
	$stmt->close();
	closeConnection($db);
	$str = rtrim($str, ",");
	$str .= "]}";
	return $str;
}

?>