# PHP-Unserialize

Common Lisp system for parsing PHP-serialized data. 

## Installation

This system can be installed from [UltraLisp](https://ultralisp.org/) like this:

```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload "php-unserialize")
```

## Usage

```common-lisp
(php-unserialize:parse "i:512;a:4:{i:0;s:3:\"Red\";i:1;s:5:\"Green\";i:2;s:4:\"Blue\";i:3;a:3:{i:0;b:1;i:1;d:2.31;i:2;O:11:\"personClass\":1:{s:3:\"age\";i:12;}}}")
;; => (512 ((0 . "Red") (1 . "Green") (2 . "Blue")
;;      (3 (0 . T) (1 . 2.31) (2 :OBJECT "personClass" (("age" . 12))))))
```

## Caveats

- References not supported yet

## Documentation

- [Wiki: PHP serialization format](https://en.wikipedia.org/wiki/PHP_serialization_format)
