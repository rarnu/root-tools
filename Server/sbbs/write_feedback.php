<?php

include "../database/database.php";

// write_feedback.php

$device=$_POST["devices"];
$appver=$_POST["appver"];
$osver=$_POST["osver"];
$email=$_POST["email"];
$userid=$_POST["userid"];
$text=$_POST["text"];

$db=openConnection();
$sql="insert into sbbs_feedback (device, appver, osver, email, userid, feedback) values ('$devices',$appver,'$osver','$email','$userid','$text')";
$result=query($db, $sql);
closeConnection($db);
echo $result;

?>
