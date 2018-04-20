--TEST--
FFI 005: Array assignment
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php 
$m = FFI::new("int[2][2]");
$v = FFI::new("int[2]");
$v[1] = 42;
$m[1] = $v;
var_dump($m);
?>
--EXPECTF--
object(FFI\CData)#%d (2) {
  [0]=>
  object(FFI\CData)#%d (2) {
    [0]=>
    int(0)
    [1]=>
    int(0)
  }
  [1]=>
  object(FFI\CData)#%d (2) {
    [0]=>
    int(0)
    [1]=>
    int(42)
  }
}
