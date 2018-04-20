--TEST--
FFI 015: Incomplete type usage
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
try {
	new FFI("struct DIR; static struct DIR dir;");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct DIR; static struct DIR *ptr;");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct DIR; typedef struct DIR DIR; static DIR dir;");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct DIR; typedef struct DIR DIR; static DIR *ptr;");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct DIR; static struct DIR foo();");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct DIR; static struct DIR* foo();");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct DIR; static void foo(struct DIR);");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
try {
	new FFI("struct DIR; static void foo(struct DIR*);");
	echo "ok\n";
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
?>
ok
--EXPECT--
FFI\ParserException: incomplete 'struct DIR' at line 1
ok
FFI\ParserException: incomplete 'struct DIR' at line 1
ok
FFI\ParserException: incomplete 'struct DIR' at line 1
ok
FFI\ParserException: incomplete 'struct DIR' at line 1
ok
ok
