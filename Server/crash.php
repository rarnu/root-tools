<?php

include "database.php";

$deviceId = $_POST["deviceId"];
$module = $_POST["module"];
$os_version = $_POST["os_version"];
$mail = $_POST["mail"];
$build_desc = $_POST["build_desc"];
$crash = $_POST["crash"];

$result = doLog($deviceId, $module, $os_version, $mail, $build_desc, $crash);
echo $result;

function doLog($d, $m, $o, $mail, $b, $crash) {
	$nid = generateId("root_tools_crash","id");
	date_default_timezone_set("Asia/Hong_Kong");
	$sql = "insert into root_tools_crash values ('".$nid."', '".$d."', '".$m."', '".$o."', '".$mail."', '".$b."', '".$crash."', '".date("Y-m-d h:i a")."')";
	$db = openConnection();
	$str = query($db, $sql);
	closeConnection($db);
	$str = "{\"result\":\"".$str."\"}";
	return $str;
}

?>
