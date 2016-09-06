<?php

$arr = parse_ini_file("version.ini");
$ret = "{";
$ret .= "\"versionCode\":\"".$arr["versionCode"]."\",";
$ret .= "\"versionName\":".json_encode($arr["versionName"]).",";
$ret .= "\"url\":".json_encode($arr["url"]).",";
$ret .= "\"description\":".json_encode($arr["description"]);
$ret .= "}";
echo $ret;

?>
