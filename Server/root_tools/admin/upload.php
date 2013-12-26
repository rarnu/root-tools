<?php

include "../../database/database.php";

$name = $_GET["name"];
$package_name = $_GET["package_name"];
$unix_name = $_GET["unix_name"];
$icon = $_GET["icon"];
$apk = $_GET["apk"];
// mode=0: add
// mode=1: update
$mode = $_GET["mode"];

if (empty($name) || empty($package_name) || empty($apk)) {
	echo "1";
    exit;
}

$ret = DoUpload($name, $package_name, $unix_name, $icon, $apk, $mode, $aid);
echo "<br>ret: $ret";

function DoUpload($n, $p, $u, $i, $a, $m, $ai) {
	$ret = "";
    $str = "";
    if ($m == 1) {
        // update
        $db = openConnection();
        $sql = "update root_tools_recommand set name='$n',icon_url='$i',download_url='$a' where package_name='$p'";
        $str = query($db, $sql);
        closeConnection($db);
    } else {
        // add
	    $id = generateId("root_tools_recommand", "id");
        $db = openConnection();
	    $sql = "insert into root_tools_recommand (id, name, package_name, main_activity, icon_url, download_url, unix_name, app_order) values ($id, '$n', '$p', 'null', '$i', '$a', '$u', 0)";
		$str = query($db, $sql);
        closeConnection($db);
    }

	if ($str == "0") {
		$ret = "1";
	} else {
		$ret = "0";
	}
    return $ret;

}


?>
