<?php

$page_text = file_get_contents("./uploading.php");
echo $page_text;

$ver = $_POST["version"];
$db = $_FILES["mdb"];
$newver = ((int)$ver) + 1;
$filename = "YGODATA.MDB";
move_uploaded_file($db["tmp_name"], "./" . $filename);
$ret = shell_exec("python ./yugioh_import.py W ./YGODATA.MDB ${newver} ./");
header("Location: index.php");

?>