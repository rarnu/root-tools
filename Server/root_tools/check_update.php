<?php
$version = $_GET["version"];
$last_ver = "359";
$last_ver_name = "6.0.3";
$last_file = "RootTools_6.0.3.apk";
$last_size = "4.5M";

$ver_int = intval($version);

if ($ver_int < $last_ver) {
	$result = "{\"result\":\"1\",\"version_code\":\"".$last_ver."\",\"version_name\":\"".$last_ver_name."\",\"file\":\"".$last_file."\",\"size\":\"".$last_size."\"}";
} else {
	$result = "{\"result\":\"0\",\"version_code\":\"0\",\"version_name\":\"\",\"file\":\"\",\"size\":\"\"}";
}


echo $result;
?>
