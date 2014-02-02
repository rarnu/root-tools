<?php

include "../database/database.php";

// upload_gont.php?name={1}&top={2}
$name = $_GET["name"];
$top = $_GET["top"];

$ret = doUploadFont($name, $top);
echo $ret;

function doUploadFont($n, $t) {
    $dbRet = 0;
    $sql = "select id from fonts where name='$n'";
    $db = openConnection();
    $result = query($db, $sql);
    $hasRecord = 0;
    while (list($id)=mysql_fetch_row($result)) {
        $hasRecord = 1;
        break;
    }
    if ($hasRecord == 0) {
        $istop = 0;
        if ($t == "1") {
            $istop = 1;
        }
        $fileName = $n.".ttf";
        $previewName = $n.".png";
        $sql = "insert into fonts (name, filename, istop, preview) values ('$n','$fileName',$istop,'$previewName')";
        $result = query($db, $sql);
        $dbRet = $result;
    }
    closeConnection($db);
    return $dbRet;
}

?>
