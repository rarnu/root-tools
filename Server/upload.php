<?php
include "./database.php";

// upload.php?name={1}&package_name={2}&main_activity={3}&unix_name={4}
// [attach:FILES("icon")] [attach:FILES("apk")]

$name = $_POST["name"];
$package_name = $_POST["package_name"];
$main_activity = $_POST["main_activity"];
$unix_name = $_POST["unix_name"];
$icon = $_FILES["icon"];
$apk = $_FILES["apk"];

if (empty($name) || empty($package_name) || empty($main_activity) || empty($unix_name)) {
	header("Location:field_error.php");
	exit;
}

$ret = DoUpload($name, $package_name, $main_activity, $unix_name, $icon, $apk);
if ($ret == "0") {
	header("Location:upload_succ.php");
} else {
	header("Location:upload_fail.php");
}

function DoUpload($n, $p, $m, $u, $fi, $fp) {
	
	$ret = "";
	$err_i = $fi["error"];
	$err_p = $fp["error"];
	if ($err_i > 0 || $err_p > 0) {
		$ret = "1";
	} else {
		$iconUrl = DoSaveFile("icon", $fi);
		$downloadUrl = DoSaveFile("package", $fp);
		$id = generateId("root_tools_recommand", "id");
		$db = openConnection();
		$sql = "insert into root_tools_recommand (id, name, package_name, main_activity, icon_url, download_url, unix_name, app_order) values ($id, '$n', '$p', '$m', '$iconUrl', '$downloadUrl', '$u', 0)";
		echo $sql;
		$str = query($db, $sql);
		closeConnection($db);
		echo "sql result: $str";
		if ($str == "0") {
			$ret = "1";
		} else {
			$ret = "0";
		}
		
  	}
	
	return $ret;

}

function DoSaveFile($dir, $file) {
	$filename = $file["name"];
	move_uploaded_file($file["tmp_name"], "./$dir/$filename");
	return $filename;

}
?>
