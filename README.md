# FFI PHP extension (Foreign Function Interface)

FFI PHP extension provides a simple way to call native functions, access native variables and create/access data structures defined in C language. The API of the extension is very simple and demonstrated by the following example and its output.

```php
<?php
$libc = new FFI("
    int printf(const char *format, ...);
    const char * getenv(const char *);
    unsigned int time(unsigned int *);

    typedef unsigned int time_t;
    typedef unsigned int suseconds_t;

    struct timeval {
        time_t      tv_sec;
        suseconds_t tv_usec;
    };

    struct timezone {
        int tz_minuteswest;
        int tz_dsttime;
    };

	int gettimeofday(struct timeval *tv, struct timezone *tz);    
", "libc.so.6");

$libc->printf("Hello World from %s!\n", "PHP");
var_dump($libc->getenv("PATH"));
var_dump($libc->time(null));

$tv = $libc->new("struct timeval");
$tz = $libc->new("struct timezone");
$libc->gettimeofday(FFI::addr($tv), FFI::addr($tz));
var_dump($tv->tv_sec, $tv->tv_usec, $tz);
?>
```

```
Hello World from PHP!
string(135) "/usr/lib64/qt-3.3/bin:/usr/lib64/ccache:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/home/dmitry/.local/bin:/home/dmitry/bin"
int(1523617815)
int(1523617815)
int(977765)
object(FFI\CData:<struct>)#3 (2) {
  ["tz_minuteswest"]=>
  int(-180)
  ["tz_dsttime"]=>
  int(0)
}
```

FFI::\__constructor() takes two arguments (both are optional). The first one is a collection of C declarations and the second is DSO library. All variables and functions defined by first arguments are bound to corresponding native symbols in DSO library and then may be accessed as FFI object methods and properties. C types of argument, return value and variables are automatically converted to/from PHP types (if possible). Otherwise, they are wrapped in a special CData proxy object and may be accessed by elements.

In some cases (e.g. passing C structure by pointer) we may need to create a real C data structures. This is possible using FFF::new() method. It takes a C type definition and may reuse C types and tags defined by FFI::\__constructor().

It's also possible to use FFI::new() as a static method to create arbitrary C data structures.

``` php
<?php
$p = FFI::new("struct {int x,y;} [2]");
$p[0]->x = 5;
$p[1]->y = 10;
var_dump($p);
```

```
object(FFI\CData:<struct>[2])#1 (2) {
  [0]=>
  object(FFI\CData:<struct>)#2 (2) {
    ["x"]=>
    int(5)
    ["y"]=>
    int(0)
  }
  [1]=>
  object(FFI\CData:<struct>)#3 (2) {
    ["x"]=>
    int(0)
    ["y"]=>
    int(10)
  }
}
```

### API Reference

##### function FFI::__construct([string $cdef = "" [, string $lib = null]])

##### Call Native Functions

All functions defined in FFI constructor may be called as methods of the created FFI object.

```php
$libc = new FFI("const char * getenv(const char *);", "libc.so.6");
var_dump($libc->getenv("PATH"));
```

##### Read/Write Values of Native Variables

All functions defined in FFI constructor may be accessed as properties of the created FFI object.

```php
$libc = new FFI("extern int errno;", "libc.so.6");
var_dump($libc->errno);
```

##### function FFI::new(string $type = "" [, bool $owned = true]): FFI\CData

This function may be used to create a native data structure. The first argument is a C type definition. The following example creates two dimensional array of integers.

```php
$p = FFI::new("int[2][2]");
var_dump($p, FFI::sizeof($p));
```

FFI::new() may be called statically and use only predefined types, or as a method of previously created FFI object. In last case the first argument may reuse all type and tag names defined in FFI constructor. By default FFI::new() creates "owned" native data structures, that live together with corresponding PHP object, reusing PHP reference-counting and GC. However, in some cases it may be necessary to manually control the life time of the data structure. In this case, the second argument should be set to "false" and then the created object should be manually deallocated using FFI::free(). 

##### static function FFI::free(FFI\CData $cdata): void

manually removes previously created "unowned" data structure.

##### Read/Write Elements of Native Arrays

Elements of native array may be accessed in the same way as elements of PHP arrays. Of course, native arrays support only integer indexes. It's not possible to check element existence using isset() or empty() and remove element using unset(). Native arrays work fine with "foreach" statement.

```php
$p = FFI::new("int[2]");
$p[0] = 1;
$p[1] = 2;
foreach ($p as $key => $val) {
	echo "$key => $val\n";
}
```

##### Read/Write Fields of Native "struct" or "union"

Fields of native struct/union may be accessed in the same way as properties of PHP objects. It's not possible to check filed existence using isset() or empty(), remove them using unset(), and iterate using "foreach" statement.

```php
$pp = FFI::new("struct {int x,y;}[2]");
foreach($pp as $n => &$p) {
	$p->x = $p->y = $n;
}
var_dump($pp);
```

##### function FFI::cast(string $type = "" , FFI\CData $cdata): FFI\CData

...

##### static function FFI::sizeof(FFI\CData $cdata): int

returns the size of corresponding native data structure.

##### static function FFI::alignof(FFI\CData $cdata): int

returns the alignment of corresponding native data structure.

##### static function FFI::memcpy(FFI\CData $dst, $src, int $size): void

copies $size bytes from memory area $src to memory area $dst. $src may be any native data structure or PHP string.

##### static function FFI::memcmp($src1, $src2, int $size): int

compares $size bytes from memory area $src1 and $dst2. $src1 and $src2 may be any native data structures or PHP strings.

##### static function FFI::memset(FFI\CData $dst, int $c, int $size): void

fills the $size bytes of the memory area pointed to by $dst with the constant byte $c

##### static function FFI::string(FFI\CData $src [, int $size]): string

creates a PHP string from $size bytes of memory area pointed by $src. If size is omitted, $src must be zero terminated.

##### Native Pointers

...

##### Calling Native Closures

...

##### PHP Callbacks

It's possible to assign PHP function to native function variable. This seems to work, but this functionality is not supported on all libffi platforms, it is not efficient and leaks resources by the end of request.

### Status

In current state, access to FFI data structures is significantly (about 2 times) slower, than access to PHP arrays and objects. It doesn't make no sense to use them for speed, but may make sense to reduce memory consumption.

### Requiremnt

- php-master (7.3)
- [libffi-3.*](http://sourceware.org/libffi/)

### Install

``` bash
phpize
./configure --with-ffi
make
sudo make install
```
