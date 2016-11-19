<?php

require_once "database.php";

$nickname = $_REQUEST["nickname"];
$comment = $_REQUEST["comment"];
$photo1 = $_FILES["photo1"];
$photo2 = $_FILES["photo2"];
$photo3 = $_FILES["photo3"];
$photo4 = $_FILES["photo4"];
$photo5 = $_FILES["photo5"];

$ret = DoQuery($nickname, $comment, $photo1, $photo2, $photo3, $photo4, $photo5);
echo $ret;

function DoQuery($n, $c, $p1, $p2, $p3, $p4, $p5) {
	$str = "{\"result\":1}";
	date_default_timezone_set("Asia/Hong_Kong");
	$t_str = date("YmdHis");
	$commit_date = date("Y-m-d H:i:s");
	$pname = generateToken().".${t_str}.";
	$path1 = "./files/${pname}1";
	$path2 = "./files/${pname}2";
	$path3 = "./files/${pname}3";
	$path4 = "./files/${pname}4";
	$path5 = "./files/${pname}5";
	$dbp1 = "";
	$dbp2 = "";
	$dbp3 = "";
	$dbp4 = "";
	$dbp5 = "";
	if (isset($p1)) { move_uploaded_file($p1["tmp_name"], $path1); $dbp1 = "${pname}1"; }
	if (isset($p2)) { move_uploaded_file($p2["tmp_name"], $path2); $dbp2 = "${pname}2"; }
	if (isset($p3)) { move_uploaded_file($p3["tmp_name"], $path3); $dbp3 = "${pname}3"; }
	if (isset($p4)) { move_uploaded_file($p4["tmp_name"], $path4); $dbp4 = "${pname}4"; }
	if (isset($p5)) { move_uploaded_file($p5["tmp_name"], $path5); $dbp5 = "${pname}5"; }
	$db = openConnection();
	$stmt = $db->prepare("insert into feedback(nickname, comment, photo1, photo2, photo3, photo4, photo5, commit_date) values (?, ?, ?, ?, ?, ?, ?, ?)");
	$stmt->bind_param("ssssssss", $n, $c, $dbp1, $dbp2, $dbp3, $dbp4, $dbp5, $commit_date);
	$stmt->execute();
	$rows = intval($stmt->affected_rows);
	$stmt->close();
	closeConnection($db);
	if ($rows != 0) {
		$str = "{\"result\":0}";
	}
	return $str;	
}

?>
