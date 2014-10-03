<?php

// android
$lastApkVersion=106;
$lastApkVersionName="ver 3.0.6";
$lastApkCardId=6666;
$lastApkDbVer=102;

// ios
$lastIpaVersion=106;
$lastIpaVersionName="ver 3.0.6";
$lastIpaCardId=6666;
$lastIpaDbVer=102;

// ================================
$version=$_GET["ver"];
$cardId=$_GET["cardid"];
$dbver=$_GET["dbver"];
$os=$_GET["os"];
if (empty($dbver)) {
    $dbver = 0;
}
if (empty($os)) {
    $os = "a";
}

if ($os == "a") {
    // android
    $apkUpdate=0;
    if ($lastApkVersion > $version) {
        $apkUpdate = 1;
    }
    $apkDataUpdate=0;
    if ($dbver != 0) {
        if ($lastApkDbVer > $dbver) {
            $apkDataUpdate=1;
        }
    } else {
        if ($lastApkCardId > $cardId) {
            $apkDataUpdate=1;
        }
    }
    $newCardCount = $lastApkCardId - $cardId;
    $str="{\"apk\":\"$apkUpdate\",\"data\":\"$apkDataUpdate\",\"newcard\":\"$newCardCount\",\"apkversion\":\"$lastApkVersionName\"}";
    echo $str;
} else {
    // ios
    $ipaUpdate=0;
    if ($lastIpaVersion > $version) {
        $ipaUpdate = 1;
    }
    $ipaDataUpdate=0;
    if ($dbver != 0) {
        if ($lastIpaDbVer > $dbver) {
            $ipaDataUpdate=1;
        }
    } else {
        if ($lastIpaCardId > $cardId) {
            $ipaDataUpdate=1;
        }
    }
    $newCardCount = $lastIpaCardId - $cardId;
    $str="{\"ipa\":\"$ipaUpdate\",\"data\":\"$ipaDataUpdate\",\"newcard\":\"$newCardCount\",\"ipaversion\":\"$lastIpaVersionName\"}";
    echo $str;
}

?>
