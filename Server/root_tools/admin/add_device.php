<?php
include "../../database/database.php";
$device=$_POST["device"];
$username=$_POST["username"];

$ret=doAddDevice($device, $username);
header("Location: developer.php");

function doAddDevice($d, $u) {
    $db = openConnection();
    $sql = "insert into root_tools_update_user (device, user_memo) values ('$d', '$u')";
    $result = query($db, $sql);
    closeConnection($db);
    return $result;
}


?>
