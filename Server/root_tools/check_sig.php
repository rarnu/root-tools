<?php
include "../database/database.php";

// check_sig.php?sig={1}
$sig = $_GET["sig"];
if (empty($sig) || $sig == "") {
    echo "{\"result\":1}";
} else {
    $ret = doCheckSig($sig);
    echo $ret;
}

function doCheckSig($s) {
    $db = openConnection();
    $sql = "select * from root_tools_sig order by id desc limit 0,1";
    $result = query($db, $sql);
    $realSig="";
    
    while (list($id, $signature)=mysql_fetch_row($result)) {
        $realSig = $signature;
        break;
    }
    closeConnection($db);
    $str = "{\"result\":1}";
    if ($realSig == $s) {
        $str = "{\"result\":0}";
    }
    return $str;
}

?>
