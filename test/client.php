<?php

$socket = fsockopen("unix:///tmp/misocketlindo.sock", -1, $errno, $errstr);
fwrite($socket, "hola desde php =)");