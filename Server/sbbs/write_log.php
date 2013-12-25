<?php

include "../database/database.php";

// write_log.php

$device=$_POST["devices"];
$appver=$_POST["appver"];
$osver=$_POST["osver"];
$email=$_POST["email"];
$action=$_POST["action"];
$detail=$_POST["detail"];
$actiontime=$_POST["actiontime"];

$db=openConnection();
$sql="insert into sbbs_log (device, appver, osver, email, action, detail, actiontime) values ('$devices',$appver,'$osver','$email','$action','$detail','$actiontime')";
$result=query($db, $sql);
closeConnection($db);
echo $result;

?>
