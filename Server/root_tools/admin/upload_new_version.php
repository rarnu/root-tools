<?php

include "../../database/database.php";

$name=$_POST["name"];
$ver_code=$_POST["ver_code"];
$size=$_POST["size"];
$filename=$_POST["filename"];
$update_desc=$_POST["update_desc"];

$ret=doUploadNewVersion($name, $ver_code, $size, $filename, $update_desc);
echo $ret;

function doUploadNewVersion($n, $v, $s, $f, $u) {
    $db=openConnection();
    $sql = "insert into root_tools_update (name, ver_code, size, filename, update_desc) values ('$n',$v,'$s','$f','$u')";
    $result=query($db, $sql);
    closeConnection($db);
    return $result;
}

?>
