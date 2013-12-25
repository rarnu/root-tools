<?php
$version = $_GET["version"];
$last_ver = "140";
$last_ver_name = "5.4.0";
$last_file = "RootTools_5.4.0.apk";
$last_size = "3.7M";

$ver_int = intval($version);

if ($ver_int < $last_ver) {
	$result = "{\"result\":\"1\",\"version_code\":\"".$last_ver."\",\"version_name\":\"".$last_ver_name."\",\"file\":\"".$last_file."\",\"size\":\"".$last_size."\"}";
} else {
	$result = "{\"result\":\"0\",\"version_code\":\"0\",\"version_name\":\"\",\"file\":\"\",\"size\":\"\"}";
}


echo $result;
?>
