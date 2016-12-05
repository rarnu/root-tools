<?php

require_once "database.php";

$type = $_REQUEST["type"];

$ret = DoQuery($type);
echo $ret;

function DoQuery($t) {
	$str = "{\"result\":0, \"data\":[";
	$db = openConnection();
	$stmt = $db->prepare("select id, model, sdk, appver, commit_date, data from crash where status = ? order by id desc");
	$stmt->bind_param("i", $t);
	$stmt->execute();
	$id = 0;
	$model = "";
	$sdk = 0;
	$appver = 0;
	$commit_date = "";
	$data = "";
	$stmt->bind_result($id, $model, $sdk, $appver, $commit_date, $data);
	while ($stmt->fetch()) {
		$str .= "{\"id\":${id},\"model\":".json_encode($model).",\"sdk\":${sdk},\"appver\":${appver},\"time\":\"${commit_date}\",\"data\":".json_encode($data)."},";
	}
	$stmt->close();
	closeConnection($db);
	$str = rtrim($str, ",");
	$str .= "]}";
	return $str;
}

?>