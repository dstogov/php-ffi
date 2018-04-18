--TEST--
FFI 100: PHP symbols
--SKIPIF--
<?php require_once('skipif.inc'); ?>
<?php
try {
	new FFI("extern void *zend_printf;");
} catch (Throwable $e) {
	die('skip PHP symbols not available');
}
?>
--FILE--
<?php
$zend = new FFI("
	const char *get_zend_version(void);
	//char *get_zend_version(void);
	extern size_t (*zend_printf)(const char *format, ...);
");
var_dump(trim($zend->get_zend_version()));
//var_dump(trim(FFI::string($zend->get_zend_version())));
var_dump($zend->zend_printf);
var_dump(($zend->zend_printf)("Hello %s!\n", "World"));
?>
--EXPECTF--
string(%d) "Zend Engine %s"
object(CData)#%d (1) {
  ["cptr"]=>
  object(CData)#%d (0) {
  }
}
Hello World!
int(13)
