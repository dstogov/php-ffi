--TEST--
FFI 029: _Alignas
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
$ffi = new FFI("
	typedef char t1;
	typedef char _Alignas(int) t2;
");
var_dump(FFI::sizeof($ffi->new("struct {char a; t1 b;}")));
var_dump(FFI::sizeof($ffi->new("struct {char a; t2 b;}")));
?>
--EXPECT--
int(2)
int(8)
