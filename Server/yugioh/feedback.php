<?php

include "../database/database.php";
// feedback.php?id={1}&email={2}&text={3}&appver={4}&osver={5}

$id=$_GET["id"];
$email=$_GET["email"];
$text=$_GET["text"];
$appver=$_GET["appver"];
$osver=$_GET["osver"];

$ret = doRecordFeedback($id, $email, $text, $appver, $osver);
echo $ret;

function doRecordFeedback($i,$e,$t,$a,$o) {
    $db = openConnection();
    $sql = "insert into yugioh_feedback (device, email, feedback, appver, osver) values ('$i','$e','$t',$a,$o)";
    $result = query($db, $sql);
    closeConnection($db);
    return $result;
}

?>
