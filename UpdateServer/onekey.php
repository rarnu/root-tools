<?php

$action = $_REQUEST["action"];
$pkg = $_REQUEST["pkg"];
$ver = $_REQUEST["ver"];

if ($action == "get") {
    $filePath = "./profile/${pkg}/${ver}.lst";
    $str = file_get_contents($filePath);
    echo $str;
} else if ($action == "put") {
    $data = $_REQUEST["data"];
    $d = date("Ymd_His");
    mkdir("./upfile/${pkg}", 0777, true);
    $filePath = "./upfile/${pkg}/${ver}_${}.lst";
    file_put_contents($filePath, $data);
    echo "OK";
}

?>