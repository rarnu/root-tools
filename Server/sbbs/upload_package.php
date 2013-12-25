<?php

include "../database/database.php";
// upload_pacakge.php
$mode=$_POST["mode"];
$version=$_POST["version"];
$versionName=$_POST["version_name"];
$updateLog=$_POST["update_log"];
$size=$_POST["size"];
$publicTime=date("Y-m-d H:i:s",time());
$fileName="SbbsMeClient_".date("Ymd",time()).".apk";
$file=$_FILES["file"];
$url="http://rarnu.7thgen.info/sbbs/download/".$fileName;
$ret=0;
if ($file["error"] > 0 || $version=="" || $versionName=="" || $size=="") {
    $ret = 1;
} else {
    
    // save file on server
    move_uploaded_file($file["tmp_name"],"./download/$fileName");
    $sql="insert into sbbs_version (type,version,version_name,update_log,size,public_time,url) values ($mode,$version,'$versionName','$updateLog','$size','$publicTime','$url')";
    $db = openConnection();
    $result = query($db, $sql);
    if ($result=="0") {
        $ret = 1;
    } else {
        $ret = 0;
    }
    closeConnection($db);
    
}

if ($mode==0) {
    if ($ret==0) {
        header("Location:dailybuild.php");
    } else {
        header("Location:upload_error.php?mode=0");
    }
} else if ($mode==1) {
    if ($ret==0) {
        header("Location:rcbuild.php");
    } else {
        header("Location:upload_error.php?mode=1");
    }
}



?>
