<?php

include "./database.php";

// save_log.php?device={1}&page={2}&click={3}&module={4}

$device = $_GET["device"];
$page = $_GET["page"];
$click = $_GET["click"];
$module = $_GET["module"];

$ret = DoSaveLog($device, $page, $click, $module);
echo $ret;

function DoSaveLog($d, $p, $c, $m) {

	$id = generateId("lovingyou_log", "id");
	$db = openConnection();
	date_default_timezone_set("Asia/Hong_Kong");
	$cd = date("Y-m-d h:i a");
	$ct = time();
	$sql = "insert into lovingyou_log (id, device, page, click, c_date, c_time, module) values ($id, '$d', '$p', '$c', '$cd', '$ct', '$m')";
	$result = query($db, $sql);
	closeConnection($db);
	return $result;
}

?>
