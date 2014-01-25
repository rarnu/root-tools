<?php
include "../database/database.php";

$ret = doGetTopFonts();
echo $ret;

function doGetTopFonts() {
    $sql = "select name,filename from fonts where istop=1 order by id asc";
    $db = openConnection();
    $result = query($db, $sql);
    closeConnection($db);
    $str = "[";
    while (list($name, $filename)=mysql_fetch_row($result)) {
        $str = $str."{\"name\":".json_encode($name).",\"file\":".json_encode($filename)."},";
    }
    $str = rtrim($str, ",");
    $str = $str."]";
    return $str;
}

?>
