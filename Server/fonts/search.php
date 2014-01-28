<?php
include "../database/database.php";
// search.php?name={1}

$font_name=$_GET["name"];
$ret = "";
if (empty($font_name)) {
    $ret = "[]";
} else {
    $ret = doSearchFonts($font_name);
}
echo $ret;

function doSearchFonts($f) {
    $sql = "select name,filename,preview from fonts where name like '%$f%'";
    $db = openConnection();
    $result = query($db, $sql);
    closeConnection($db);
    $str = "[";
    while (list($name, $filename,$preview)=mysql_fetch_row($result)) {
        $str = $str."{\"name\":".json_encode($name).",\"file\":".json_encode($filename).",\"preview\":".json_encode($preview)."},";
    }
    $str = rtrim($str, ",");
    $str = $str."]";
    return $str;
}

?>
