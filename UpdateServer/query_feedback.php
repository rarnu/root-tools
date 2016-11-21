<?php

require_once "database.php";

$type = $_REQUEST["type"];
$ret = DoQuery($type);
echo $ret;

function DoQuery($t) {
	$str = "{\"result\":0, \"data\":[";
	$db = openConnection();
	$stmt = $db->prepare("select id, nickname, comment, photo1, photo2, photo3, photo4, photo5, commit_date from feedback where status = ? order by id desc");
	$stmt->bind_param("i", $t);
	$stmt->execute();
	$id = 0;
	$nickname = "";
	$comment = "";
	$photo1 = "";
	$photo2 = "";
	$photo3 = "";
	$photo4 = "";
	$photo5 = "";
	$commit_date = "";
	$stmt->bind_result($id, $nickname, $comment, $photo1, $photo2, $photo3, $photo4, $photo5, $commit_date);
	while ($stmt->fetch()) {
		$str .= "{\"id\":${id},\"nickname\":".json_encode($nickname).",\"comment\":".json_encode($comment).",\"photo1\":\"${photo1}\",\"photo2\":\"${photo2}\",\"photo3\":\"${photo3}\",\"photo4\":\"${photo4}\",\"photo5\":\"${photo5}\",\"commit_date\":\"${commit_date}\"},"; 
	}
	$stmt->close();
	closeConnection($db);
	$str = rtrim($str, ",");
	$str .= "]}";
	return $str;
}

?>
