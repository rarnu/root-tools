<?php

function openConnection() {
	$db = @mysql_connect("localhost","root","root") or die("error");
	mysql_query("SET NAMES 'utf8'", $db);
	mysql_select_db("rarnu", $db);
	return $db;
}

function query($db, $sql) {
	return mysql_query($sql, $db);
}

function closeConnection($db) {
	mysql_close($db);
}

function generateId($table, $idfield) {
	$db = openConnection();
	$sql = "select ".$idfield." from ".$table." order by ".$idfield." desc limit 0,1";
	$result = mysql_query($sql, $db);
	
	$rid = 1;
	while (list($id) = mysql_fetch_row($result)) {
		$rid = $id + 1;
		break;
	}
	closeConnection($db);
	return $rid;
}

function checkKeyValue($db, $table, $field, $value) {
	// return 0 for ok and 1 for 
	$sql = "select * from ".$table." where ".$field."='".$value."'";
	mysql_query($sql, $db);
	$rows = mysql_affected_rows($db);
	return $rows;
}

?>
