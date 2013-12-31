<?php
include "../../database/database.php";

$id=$_GET["id"];

$ret = doDeleteDevice($id);
header("Location: developer.php");

function doDeleteDevice($i) {
    $db = openConnection();
    $sql = "delete from root_tools_update_user where id=$i";
    $result=query($db, $sql);
    closeConnection($db);
    return $result;
}

?>
