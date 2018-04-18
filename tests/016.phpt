--TEST--
FFI 016: Structure constraints
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
try {
	new FFI("struct X {void x();};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct X {struct X x;};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct X {struct X *ptr;};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
?>
ok
--EXPECTF--
FFIParserException: 'function' type is not allowed at line 1
FFIParserException: struct/union can't contain an instance of itself at line 1
ok
ok
