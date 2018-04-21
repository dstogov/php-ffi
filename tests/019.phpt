--TEST--
FFI 019: Parameter type adjustment
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
try {
	new FFI("static int foo(int[]);");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("static int foo(int bar(int));");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
?>
ok
--EXPECT--
ok
ok
ok
