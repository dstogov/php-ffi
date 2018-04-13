# FFI PHP extension (Foreign Function Interface)

FFI PHP extension provides a simple way to call native functions, access native variables and create/access data structures defined in C language. The API of the extension is very simple and demonstrated by the following example and its output.

```php
<?php
$libc = new FFI("
    int printf(const char *format, ...);
    char * getenv(const char *);
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
$libc->gettimeofday($tv, $tz);
var_dump($tv-tv_sec, $tv->tv_usec);
?>
```

```
Hello World from PHP!
string(135) "/usr/lib64/qt-3.3/bin:/usr/lib64/ccache:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/home/dmitry/.local/bin:/home/dmitry/bin"
int(1523617815)
int(1523617815)
int(977765)
object(CData)#3 (2) {
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
object(CData)#1 (2) {
  [0]=>
  object(CData)#2 (2) {
    ["x"]=>
    int(5)
    ["y"]=>
    int(0)
  }
  [1]=>
  object(CData)#3 (2) {
    ["x"]=>
    int(0)
    ["y"]=>
    int(10)
  }
}
```

### Status

The extension is experimental and in the very early stage of development (tested only on Linux).

In current state, access to FFI data structures is significantly (about 4 times) slower, than access to PHP arrays and objects. it make no sense to use them for speed, but may make sense to use to reduce memory consumption.

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
