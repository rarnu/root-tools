<?php

require_once "Parsedown.php";

$parsedown = new Parsedown();
$md = file_get_contents("help.md");
echo $parsedown->text($md);

?>
