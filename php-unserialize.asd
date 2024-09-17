(defsystem php-unserialize
  :version "0.2.0"
  :author "Dmitrii Kosenkov"
  :license "MIT"
  :depends-on ("smug" "parse-float")
  :description "Parser for PHP serialization format"
  :homepage "https://github.com/Junker/php-unserialize"
  :source-control (:git "https://github.com/Junker/php-unserialize.git")
  :components ((:file "php-unserialize")))
