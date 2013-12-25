<?php

include "../database/database.php";
// add_require.php
$mode=$_POST["mode"];
$id=$_POST["id"];
$name=$_POST["name"];
$body=$_POST["body"];
$elevel=$_POST["elevel"];
$version=$_POST["version"];

if ($name==="" || $body==="") {
    header("Location:add_error.php");
    return;
}

$db=openConnection();
$sql = "";
if ($mode==0) {
    $sql="insert into sbbs_require (name, body, elevel, stat, version) values ('$name', '$body', $elevel, 2, '$version')";
} else {
    $sql = "update sbbs_require set name='$name', body='$body', elevel=$elevel, version='$version' where id=$id";
}
echo $sql;
$result=query($db, $sql);
closeConnection($db);

header("Location:require.php");
?>
