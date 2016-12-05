<?php

require_once "database.php";

$model = $_REQUEST["model"];
$sdk = $_REQUEST["sdk"];
$appver = $_REQUEST["appver"];
$data = $_REQUEST["data"];

date_default_timezone_set("Asia/Hong_Kong");
$commit_date = date("Y-m-d H:i:s");
$db = openConnection();
$stmt = $db->prepare("insert into crash (model, sdk, appver, commit_date, data) values (?, ?, ?, ?, ?)");
$stmt->bind_param("siiss", $model, $sdk, $appver, $commit_date, $data);
$stmt->execute();
$stmt->close();
closeConnection($db);

?>