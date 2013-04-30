<?php
$version = $_GET["version"];
$last_ver = 20;
$last_ver_name = "4.0.0";
$last_file = "RootTools_4.0.0.apk";
$last_size = "2M";

$ver_int = intval($version);

if ($ver_int < $last_ver) {
	$result = "{\"result\":\"1\",\"version_code\":\"".$last_ver."\",\"version_name\":\"".$last_ver_name."\",\"file\":\"".$last_file."\",\"size\":\"".$last_size."\"}";
} else {
	$result = "{\"result\":\"0\",\"version_code\":\"0\",\"version_name\":\"\",\"file\":\"\",\"size\":\"\"}";
}


echo $result;
?>
