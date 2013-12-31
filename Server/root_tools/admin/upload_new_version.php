<?php

include "../../database/database.php";

$name=$_POST["name"];
$ver_code=$_POST["ver_code"];
$size=$_POST["size"];
$filename=$_POST["filename"];
$update_desc=$_POST["update_desc"];
$tag = $_POST["tag"];

$ret=doUploadNewVersion($name, $ver_code, $size, $filename, $update_desc,$tag);
if ($ret == 0) {
    header("Location: upload_fail.php");
} else {
    header("Location: upload_succ.php");
}

function doUploadNewVersion($n, $v, $s, $f, $u,$t) {
    $db=openConnection();
    $sql = "insert into root_tools_update (name, ver_code, size, filename, update_desc,tag) values ('$n',$v,'$s','$f','$u',$t)";
    $result=query($db, $sql);
    closeConnection($db);
    return $result;
}

?>
