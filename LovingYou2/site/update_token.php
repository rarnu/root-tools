<?php

include "database.php";

// update_token.php?token={1}&time={2}

$token=$_GET["token"];
$time=$_GET["time"];

$ret = DoUpdateToken($token, $time);
echo $ret;

function DoUpdateToken($tk, $tm) {
	$db = openConnection();
	$sql = "update sina_token set token='$tk', expired_time='$tm' where _id=0";
	$str = query($db, $sql);
	closeConnection($db);
	$str = "{\"result\":\"$str\"}";
	return $str;
}

?>
