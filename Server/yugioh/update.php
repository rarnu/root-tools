<?php
$apkVersion=$_GET["ver"];
$cardId=$_GET["cardid"];
$dbver=$_GET["dbver"];

$lastApkVersion=32;
$lastVersionName="ver 2.1.1";
$lastCardId=6223;
$lastDbVer=3;

if (empty($dbver)) {
    $dbver = 0;
}

$apkUpdate=0;
if ($lastApkVersion > $apkVersion) {
	$apkUpdate = 1;
}
$dataUpdate=0;
if ($dbver != 0) {
    if ($lastDbVer > $dbver) {
        $dataUpdate=1;
    }
} else {
    if ($lastCardId > $cardId) {
	    $dataUpdate = 1;
    }
}
$newCardCount = $lastCardId - $cardId;

$version="{\"apk\":\"$apkUpdate\",\"data\":\"$dataUpdate\",\"newcard\":\"$newCardCount\",\"apkversion\":\"$lastVersionName\"}";
echo $version;

?>
