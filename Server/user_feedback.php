<?php

include "database.php";

$deviceId = $_GET["deviceId"];
$module = $_GET["module"];
$os_version = $_GET["os_version"];
$mail = $_GET["mail"];
$build_desc = $_GET["build_desc"];
$comment = $_GET["comment"];

$result = doLog($deviceId, $module, $os_version, $mail, $build_desc, $comment);
echo $result;

function doLog($d, $m, $o, $mail, $b, $comment) {
	$nid = generateId("root_tools_feedback","id");
	date_default_timezone_set("Asia/Hong_Kong");
	$sql = "insert into root_tools_feedback values ('".$nid."', '".$d."', '".$m."', '".$o."', '".$mail."', '".$b."', '".$comment."', '".date("Y-m-d h:i a")."')";
	$db = openConnection();
	$str = query($db, $sql);
	closeConnection($db);
	$str = "{\"result\":\"".$str."\"}";
	return $str;
}

?>
