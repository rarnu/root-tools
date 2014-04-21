<?php

include "../../database/database.php";

$picture = $_FILES["picture"];
$ret = doUploadNewSplash($picture);
if ($ret == 0) {
    header("Location: upload_splash_fail.php");
} else {
    header("Location: upload_splash_succ.php");
}

function doUploadNewSplash($p)
{
    $extend = get_extend($p["name"]);
    $filename = "file_" . date("YYmmddhhiiss") . "." . $extend;
    move_uploaded_file($p["tmp_name"], "../splash/" . $filename);
    $db = openConnection();
    $sql = "update root_tools_splash set filename='$filename' where id=0";
    $result = query($db, $sql);
    closeConnection($db);
    return $result;
}

function get_extend($file_name)
{
    $extend = explode(".", $file_name);
    $va = count($extend) - 1;
    return strtolower($extend[$va]);
}

?>
