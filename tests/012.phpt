--TEST--
FFI 012: serialization
--SKIPIF--
<?php require_once('skipif.inc'); ?>
--FILE--
<?php
try {
	var_dump(serialize(FFI::new("int[2]")));
} catch (Throwable $e) {
	echo get_class($e) . ": " . $e->getMessage()."\n";
}
?>
--EXPECTF--
Exception: Serialization of 'CData' is not allowed
