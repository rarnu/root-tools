<?php

require_once "database.php";

$type = $_REQUEST["type"];

$ret = DoQuery($type);
echo $ret;

function DoQuery($t) {
	$db = openConnection();
	$id = 0;
	$versionCode = 0;
	$versionName = "";
	$url = "";
	$desc = "";
	$desc_en = "";
	$ret = "{\"versionCode\":0}";
	if ($t == "last" || empty($t)) {
		$stmt = $db->prepare("select * from version order by versionCode desc limit 0, 1");
		$stmt->execute();
		$stmt->bind_result($id, $versionCode, $versionName, $url, $desc, $desc_en);
		$stmt->fetch();
		$ret = "{\"versionCode\":${versionCode}, \"versionName\":".json_encode($versionName).", \"url\":".json_encode($url).", \"description\":".json_encode($desc).",\"desc_en\":".json_encode($desc_en)."}";
		$stmt->close();
	} else if ($t == "all") {
		$ret = "{\"result\":0, \"data\":[";
		$stmt = $db->prepare("select * from version order by versionCode desc limit 0, 10");
		$stmt->execute();
		$stmt->bind_result($id, $versionCode, $versionName, $url, $desc, $desc_en);
		while($stmt->fetch()) {
			$ret .= "{\"versionCode\":${versionCode}, \"versionName\":".json_encode($versionName).", \"url\":".json_encode($url).", \"description\":".json_encode($desc).",\"desc_en\":".json_encode($desc_en)."},";
		}
		$ret = rtrim($ret, ",");
		$ret .= "]}";
	}
	closeConnection($db);
	return $ret;
}

?>
