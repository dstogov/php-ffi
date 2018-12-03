--TEST--
FFI 028: Incomplete arrays inside structure
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--INI--
ffi.enable=1
--FILE--
<?php
try {
	new FFI("struct _x {int a; int b[0];};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct _x {int a; int b[];};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct _x {int a[0]; int b;};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct _x {int a[]; int b;};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct _x { struct {int a; int b[];}; int c;};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("union _x {int a; int b[];};");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
?>
--EXPECT--
ok
ok
ok
FFI\ParserException: flexible array member not at end of struct at line 1
FFI\ParserException: flexible array member not at end of struct at line 1
FFI\ParserException: flexible array member in union at line 1
