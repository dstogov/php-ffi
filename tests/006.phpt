--TEST--
FFI 006: Pointer assignment
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php 
$v = FFI::new("int*[2]");
$v[1] = FFI::new("int");
$v[1][0] = 42;
var_dump($v);
?>
--EXPECTF--
object(CData)#%d (2) {
  [0]=>
  NULL
  [1]=>
  object(CData)#%d (1) {
    ["cptr"]=>
    int(42)
  }
}