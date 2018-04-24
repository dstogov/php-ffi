--TEST--
FFI 200: PHP callbacks
--SKIPIF--
<?php require_once('skipif.inc'); ?>
<?php
try {
	new FFI("void* zend_write;");
} catch (Throwable $e) {
	die('skip PHP symbols not available');
}
?>
--FILE--
<?php
$zend = new FFI("
	typedef int (*zend_write_func_t)(const char *str, size_t str_length);
	extern zend_write_func_t zend_write;
");

echo "Hello World!\n";

$orig_zend_write = clone $zend->zend_write;
$zend->zend_write = function($str, $len) {
	global $orig_zend_write;
	$orig_zend_write("{\n\t", 3);
	$ret = $orig_zend_write($str, $len);
	$orig_zend_write("}\n", 2);
	return $ret;
};
echo "Hello World!\n";
$zend->zend_write = $orig_zend_write;

echo "Hello World!\n";
?>
--EXPECT--
Hello World!
{
	Hello World!
}
Hello World!
