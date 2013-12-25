<?php
include "../database/database.php";

//change_stat?id={0}&stat={1}

$id=$_GET["id"];
$stat=$_GET["stat"];

$db=openConnection();
$sql="update sbbs_require set stat=$stat where id=$id";
$result=query($db, $sql);

closeConnection($db);
header("Location:require.php");

?>
