<?php

include "../database/database.php";

// get_recommand.php

$ret = DoGetRecommand();
echo $ret;

function DoGetRecommand() {
	$sql = "select * from root_tools_recommand order by app_order asc";
	$db = openConnection();
	$result = query($db, $sql);
	closeConnection($db);
	$str = "{\"data\":[";
	while (list($id, $name, $packageName, $mainActivity, $iconUrl, $downloadUrl, $unixName)=mysql_fetch_row($result)) {
		$str = $str."{\"id\":\"$id\",\"name\":".json_encode($name).",\"package_name\":\"$packageName\",\"main_activity\":\"$mainActivity\",\"icon_url\":\"$iconUrl\",\"download_url\":\"$downloadUrl\",\"unix_name\":\"$unixName\"},";
	}
	$str = rtrim($str, ",");
	$str = $str."]}";
	return $str;
}

?>
