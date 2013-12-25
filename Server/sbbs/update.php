<?php

include "../database/database.php";

$version=$_GET["version"];

$ret = DoCheckUpdate($version);
echo $ret;

function DoCheckUpdate($v) {
    $sql = "select * from sbbs_version where version > $v order by id desc limit 0,1";
    $db = openConnection();
    $result = query($db, $sql);
    $str = "{\"need_update\":false}";
    while (list($id, $type, $version, $version_name, $update_log, $size, $public_time, $url)=mysql_fetch_row($result)) {
        $str = "{\"need_update\":true,\"id\":\"$id\",\"type\":\"$type\",\"version\":\"$version\",\"version_name\":\"$version_name\",\"update_log\":\"$update_log\",\"size\":\"$size\",\"public_time\":\"$public_time\",\"url\":\"$url\"}";
        break;
    }

    closeConnection($db);
    return $str;
}

?>
