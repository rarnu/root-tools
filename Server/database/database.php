<?php

function openConnection() {
	$db = @mysql_connect("127.0.0.1:3306","root","root") or die("error");
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

function getUserNameByToken($token) {

	$str = "";
	$sql = "select nickname from lavenderplat_user where token='$token'";
	$db = openConnection();
	$result = query($db, $sql);
	while (list($nickname) = mysql_fetch_row($result)) {
		$str = $nickname;
		break;
	}
	closeConnection($db);
	return $str;
}

function getUserIdByToken($token) {
	$str = "";
	$sql = "select id from lavenderplat_user where token='$token'";
	$db = openConnection();
	$result = query($db, $sql);
	while (list($id) = mysql_fetch_row($result)) {
		$str = $id;
		break;
	}
	closeConnection($db);
	return $str;
}

function getAuthorId($id) {
	$str = "";
	$sql = "select from_user from lavenderplat_baseinfo where id=$id";
	$db = openConnection();
	$result = query($db, $sql);
	while (list($from_user) = mysql_fetch_row($result)) {
		$str = $from_user;
		break;
	}
	closeConnection($db);
	return $str;
}

function getUserIdByAccount($account) {
	$str = "";
	$sql = "select id from lavenderplat_user where account='$account'";
	$db = openConnection();
	$result = query($db, $sql);
	while (list($id) = mysql_fetch_row($result)) {
		$str = $id;
		break;
	}
	closeConnection($db);
	return $str;
}

function getUserNameByUID($uid) {
	$str = "";
	$sql = "select nickname from lavenderplat_user where id=$uid";
	$db = openConnection();
	$result = query($db, $sql);
	while (list($nickname) = mysql_fetch_row($result)) {
		$str = $nickname;
		break;
	}
	closeConnection($db);
	return $str;
}

function findRefId($master_field, $master_field_value, $detail_table, $detail_field) {
	$str = "";
	$sql = "select $detail_field from $detail_table where $master_field='$master_field_value'";
	$db = openConnection();
	$result = query($db, $sql);
	while (list($id) = mysql_fetch_row($result)) {
		$str = $id;
		break;
	}
	closeConnection($db);
	return $str;
}

function generateToken($len = 32) {
  return substr(md5(time()), rand(1,( 32 - $len)), $len);
}

function getUserAllowed($token) {
	$str = "";
	$sql = "select allow from lavenderplat_user where token='$token'";
	$db = openConnection();
	$result = query($db, $sql);
	while (list($aw) = mysql_fetch_row($result)) {
		$str = $aw;
		break;
	}
	closeConnection($db);
	return $str;
}

function substrEx($str, $from, $len) {
    return preg_replace('#^(?:[\x00-\x7F]|[\xC0-\xFF][\x80-\xBF]+){0,'.$from.'}'.
                       '((?:[\x00-\x7F]|[\xC0-\xFF][\x80-\xBF]+){0,'.$len.'}).*#s',
                       '$1',$str);
}


?>
