<?php

include "../database/database.php";

$deviceId = $_GET["deviceId"];
$module = $_GET["module"];
$os_version = $_GET["os_version"];
$mail = $_GET["mail"];
$build_desc = $_GET["build_desc"];
$comment = $_GET["comment"];
$app_version = $_GET["app_version"];

if (empty($deviceId) || empty($comment)) {
    echo "{\"result\":\"0\"}";
    return;
}

if (empty($app_version) || $app_version == null) {
    $app_version = "low";
}

$result = doLog($deviceId, $module, $os_version, $mail, $build_desc, $comment, $app_version);
echo $result;

function doLog($d, $m, $o, $mail, $b, $comment, $app) {
	$nid = generateId("root_tools_feedback","id");
	date_default_timezone_set("Asia/Hong_Kong");
	$sql = "insert into root_tools_feedback(id,deviceId,module,os_version,mail,build_desc,comment,comment_time,app_version) values ('".$nid."', '".$d."', '".$m."', '".$o."', '".$mail."', '".$b."', '".$comment."', '".date("Y-m-d h:i a")."', '$app')";
	$db = openConnection();
	$str = query($db, $sql);
	closeConnection($db);
	$str = "{\"result\":\"".$str."\"}";
	return $str;
}

?>
