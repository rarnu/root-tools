<?php

require_once "database.php";

$name = $_REQUEST["name"];
$desc = $_REQUEST["desc"];
$descEn = $_REQUEST["descEn"];
$head = $_REQUEST["head"];

$db = openConnection();
$stmt = $db->prepare("insert into thanks (name, description, description_en, head_image) values (?, ?, ?, ?)");
$stmt->bind_param("sss", $name, $desc, $descEn, $head);
$stmt->execute();
$stmt->close();
closeConnection($db);

?>