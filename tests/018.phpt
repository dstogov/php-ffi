--TEST--
FFI 018: Indirectly recursive structure
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
try {
	new FFI("struct X {struct X x[2];};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct X {struct X *ptr[2];};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
?>
ok
--EXPECTF--
FFIParserException: incomplete 'struct X' at line 1
ok
ok
