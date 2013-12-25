<?php

include "../database/database.php";
$language=$_GET["lang"];
$ret = doGetTeam($language);
echo $ret;

function doGetTeam($lng) {
    $sql = "select * from root_tools_member where lang=$lng order by id asc";
    $db = openConnection();
    $result = query($db, $sql);
    $str = "{\"member\":[";
    while (list($id,$name,$head,$position)=mysql_fetch_row($result)) {
        $str = $str."{\"id\":$id,\"name\":".json_encode($name).",\"head\":\"$head\",\"position\":".json_encode($position)."},";
    }
    $str = rtrim($str,",");
    $str = $str."],\"project\":[";
    $sql = "select * from root_tools_project where lang=$lng order by id asc";
    $result = query($db, $sql);
    while (list($id, $name, $desc)=mysql_fetch_row($result)) {
        $str = $str."{\"id\":$id,\"name\":".json_encode($name).",\"desc\":".json_encode($desc)."},";
    }
    $str = rtrim($str, ",");
    $str = $str."]}";
    closeConnection($db);
    return $str;
}

?>
