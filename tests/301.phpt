--TEST--
FFI 301: FFI loading
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
$ffi = FFI::load(__DIR__ . "/300.h");
$ffi->printf("Hello World from %s!\n", "PHP");
?>
--EXPECT--
Hello World from PHP!
