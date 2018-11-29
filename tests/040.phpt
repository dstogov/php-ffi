--TEST--
FFI 040: FFI::type() support for scalar types
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
$x = FFI::new("int");
var_dump(FFI::type($x));

$y = FFI::new("int[2]");
var_dump(FFI::type($y[0]));
?>
--EXPECTF--
object(FFI\CType:int32_t)#%d (0) {
}
object(FFI\CType:int32_t)#%d (0) {
}
