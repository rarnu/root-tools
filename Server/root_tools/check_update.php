<?php

include "../database/database.php";

$version = $_GET["version"];

$ret = doCheckUpdate($version);
echo $ret;

function doCheckUpdate($v) {
    $db = openConnection();
    $sql = "select * from root_tools_update where ver_code > $v order by id desc limit 0,1";
    $result=query($db, $sql);
    closeConnection($db);
    $str = "{\"result\":\"0\",\"version_code\":\"0\",\"version_name\":\"\",\"file\":\"\",\"size\":\"\",\"desc\":\"\"}";
    while (list($id, $name, $ver_code, $size, $filename, $update_desc)=mysql_fetch_row($result)) {
        $str = "{\"result\":\"1\",\"version_code\":\"$ver_code\",\"version_name\":\"$name\",\"file\":\"$filename\",\"size\":\"$size\",\"desc\":".json_encode($update_desc)."}";
        break;
    }
    return $str;

}

?>
