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
	$desc_en = "";
	$head = "";
	$stmt->bind_result($id, $name, $desc, $desc_en, $head);
	while ($stmt->fetch()) {
		$str .= "{\"id\":${id},\"name\":".json_encode($name).",\"desc\":".json_encode($desc).",\"desc_en\":".json_encode($desc_en).",\"head\":\"${head}\"},";
	}
	$stmt->close();
	closeConnection($db);
	$str = rtrim($str, ",");
	$str .= "]}";
	return $str;
}

?>