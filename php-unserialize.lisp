(defpackage :php-unserialize
  (:use #:cl
        #:smug
        #:parse-float)
  (:shadow #:parse)
  (:export #:parse))

(in-package :php-unserialize)

(defun .decimal ()
  (.let* ((numstr (.map 'string (.is #'digit-char-p))))
    (.identity (parse-integer numstr))))

(defun .signed-decimal ()
  (.let* ((sign (.optional (.char= #\-)))
          (num (.decimal)))
    (.identity (if sign (- num) num))))

(defun .make-string (length)
  (.let* ((list (smug::.make-list length)))
    (.identity (coerce list 'string))))

;; ==================

(defun .php-int ()
  (.prog2 (.string= "i:")
          (.signed-decimal)
          (.char= #\;)))

(defun .php-bool ()
  (.let* ((_ (.string= "b:"))
          (val (.or (.char= #\1) (.char= #\0)))
          (_ (.char= #\;)))
    (.identity (eq val #\1))))

(defun .php-float ()
  (.let* ((_ (.string= "d:"))
          (part1 (.signed-decimal))
          (_ (.char= #\.))
          (part2 (.map 'string (.is #'digit-char-p)))
          (_ (.char= #\;)))
    (.identity (parse-float (format nil "~D.~A" part1 part2)))))

(defun .php-null ()
  (.and (.string= "N")
        (.char= #\;)
        (.identity nil)))

(defun .php-reference ()
  (.let* ((_ (.or (.string= "r:") (.string= "R:")))
          (ref (.decimal))
          (_ (.char= #\;)))
    (.identity (list :reference ref))))

(defun .php-string ()
  (.let* ((_ (.string= "s:"))
          (len (.decimal)))
    (.prog2 (.string= ":\"")
            (.make-string len)
            (.string= "\";"))))

(defun .php-items (len)
  (.prog2
   (.string= "{")
   (if (> len 0)
       (.map 'list (.php-value))
       (.identity nil))
   (.string= "}")))

(defun .php-object ()
  (.let* ((_ (.string= "O:"))
          (name-len (.decimal))
          (_ (.string= ":\""))
          (name (.make-string name-len))
          (_ (.string= "\":"))
          (len (.decimal))
          (_ (.string= ":"))
          (items (.php-items len)))
    (flet ((%prepare-key (key)
             (if (stringp key)
                 (substitute #\@ #\Nul key)
                 key)))
      (.identity (list :object
                       name
                       (loop :for (key value) :on items :by #'cddr
                             :collect (cons (%prepare-key key)
                                            value))))))))

(defun .php-array ()
  (.let* ((_ (.string= "a:"))
          (len (.decimal))
          (_ (.string= ":"))
          (items (.php-items len)))
    (.identity (loop :for (key value) :on items :by #'cddr
                     :collect (cons key value)))))

(defun .php-value ()
  (.or (.php-int)
       (.php-float)
       (.php-bool)
       (.php-string)
       (.php-null)
       (.php-object)
       (.php-array)
       (.php-reference)))

(defun parse (str)
  (smug:parse (.map 'list (.php-value)) str))
