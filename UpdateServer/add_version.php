<?php

require_once "database.php";

$verCode = $_REQUEST["versionCode"];
$verName = $_REQUEST["versionName"];
$url = $_REQUEST["url"];
$desc = $_REQUEST["desc"];

$db = openConnection();
$stmt = $db->prepare("insert into version(versionCode, versionName, url, description) values (?, ?, ?, ?)");
$stmt->bind_param("isss", $verCode, $verName, $url, $desc);
$stmt->execute();
$stmt->close();
closeConnection($db);

?>