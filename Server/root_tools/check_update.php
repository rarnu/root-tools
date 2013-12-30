<?php

include "../database/database.php";

$version = $_GET["version"];
$device = $_GET["device"];

$ret = doCheckUpdate($version, $device);
echo $ret;

function doCheckUpdate($v,$d) {
    $db = openConnection();
    $sql = "";
    $user_id="";
    $str = "{\"result\":\"0\",\"version_code\":\"0\",\"version_name\":\"\",\"file\":\"\",\"size\":\"\",\"desc\":\"\"}";
    if (!empty($d)) {
        $sql = "select id from root_tools_update_user where device='$d'";
        $result = query($db, $sql);
        while (list($id)=mysql_fetch_row($result)) {
            $user_id = $id;
            break;
        }
        if ($user_id != "") {
            $sql = "select * from root_tools_update where ver_code > $v order by id desc limit 0,1";
            $result = query($db, $sql);
        }
    } else {
        $sql = "select * from root_tools_update where tag=0 and ver_code > $v order by id desc limit 0,1";
        $result=query($db, $sql);
    }
    closeConnection($db);
    while (list($id, $name, $ver_code, $size, $filename, $update_desc)=mysql_fetch_row($result)) {
        $str = "{\"result\":\"1\",\"version_code\":\"$ver_code\",\"version_name\":\"$name\",\"file\":\"$filename\",\"size\":\"$size\",\"desc\":".json_encode($update_desc)."}";
        break;
    }
    return $str;

}

?>
