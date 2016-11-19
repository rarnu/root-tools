<?php

function openConnection() {
    $db = new mysqli("127.0.0.1", "root", "root", "root_tools", 3306);
    mysqli_query($db, "SET NAMES 'utf8");
    return $db;
}

function query($db, $sql) {
    return mysqli_query($db, $sql);
}

function closeConnection($db) {
    mysqli_close($db);
}

function generateId($table, $idfield) {
    $db = openConnection();
    $sql = "select ${idfield} from ${table} order by ${idfield} desc limit 0,1";
    $stmt = $db->prepare($sql);
    $stmt->execute();
    $stmt->bind_result($id);
    $rid = 0;
    while ($stmt->fetch()) {
        $rid = $id;
        break;
    }
    $rid++;
    $stmt->close();
    closeConnection($db);
    return $rid;
}

function checkKeyValue($db, $table, $field, $value) {
    // return 0 for ok and 1 for
    $sql = "select * from " . $table . " where " . $field . "='" . $value . "'";
    mysqli_query($db, $sql);
    $rows = mysqli_affected_rows($db);
    return $rows;
}

function findRefId($master_field, $master_field_value, $detail_table, $detail_field) {
    $str = "";
    $sql = "select $detail_field from $detail_table where $master_field='$master_field_value'";
    $db = openConnection();
    $result = query($db, $sql);
    while (list($id) = mysqli_fetch_row($result)) {
        $str = $id;
        break;
    }
    closeConnection($db);
    return $str;
}

function generateToken($len = 32) {
    return substr(md5(time()), rand(1,( 32 - $len)), $len);
}

?>
