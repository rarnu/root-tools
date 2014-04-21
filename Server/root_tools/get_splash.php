<?php

include "../database/database.php";

$db = openConnection();
$sql = "select filename from root_tools_splash where id=0";
$result = query($db, $sql);
closeConnection($db);

$ret = "";
while (list($filename) = mysql_fetch_row($result)) {
    $ret = $filename;
    break;
}
echo $ret;
?>
