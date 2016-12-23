<?php

require_once "database.php";

$verCode = $_REQUEST["versionCode"];
$verName = $_REQUEST["versionName"];
$url = $_REQUEST["url"];
$desc = $_REQUEST["desc"];
$descEn = $_REQUEST["descEn"];

$db = openConnection();
$stmt = $db->prepare("insert into version(versionCode, versionName, url, description, description_en) values (?, ?, ?, ?, ?)");
$stmt->bind_param("issss", $verCode, $verName, $url, $desc, $descEn);
$stmt->execute();
$stmt->close();
closeConnection($db);

?>