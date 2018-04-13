--TEST--
FFI 001: Check if FFI is loaded
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php 
echo 'The extension "FFI" is available';
?>
--EXPECT--
The extension "FFI" is available
