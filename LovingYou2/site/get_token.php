<?php

include "database.php";

// get_token.php

$ret = DoGetToken();
echo $ret;

function DoGetToken() {
	$db = openConnection();
	$sql = "select * from sina_token where _id=0";
	$result = query($db, $sql);
	closeConnection($db);
	$str = "";
	while (list($id, $token, $time) = mysql_fetch_row($result)) {
		$str = "{\"id\":\"$id\",\"token\":\"$token\",\"time\":\"$time\"}";
		break;
	}
	return $str;
}

?>
