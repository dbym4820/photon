;; Composing command lines that don't use any special character.
(defpackage :lisp-invocation/non-special
  (:use :common-lisp :uiop)
  (:export
   #:compose-non-special-string #:compose-copy-paste-string))

(in-package :lisp-invocation/non-special)

(defun non-special-character-p (character)
  "Is this character acceptable in an eval form, even when used under Windows?"
  (let ((code (char-code character)))
    (and (< 32 code 127) (not (find character "'\\\"")))))

(defun non-special-symbol-character-p (character)
  "Is this character acceptable as part of a symbol in an eval form, even when used under Windows?"
  (let ((code (char-code character))) ;; assumes uppercase.
    ;; No "modern" syntax accepted on the test master, though it can be used on the slave.
    (and (< 32 code 91) (not (find character "'\\\":;")))))

(defun need-escape-within-pipe-p (character)
  "Does this character need to be escaped when within symbol pipe notation?"
  (or (eql character #\|) (eql character #\\)))

(defun print-string-in-pipes (string &optional s)
  "Print a symbol |escaped| between pipes with the given STRING as its name, to the output S"
  (with-output (s)
    (princ "|" s)
    (let ((string (string string)))
      (if (some 'need-escape-within-pipe-p string)
          (loop :for c :across string :do
                (when (need-escape-within-pipe-p c) (princ #\\ s))
                (princ c s))
          (princ string s)))
    (princ "|" s)))

(defun compose-non-special-string (forms &optional s)
  "Given some FORMS, compose a string suitable to be passed for evaluation
to a Lisp implementation from the shell command-line with e.g. sbcl --eval or equivalent.
The string will contain no special character (space, dollars, quotes, double quotes)
so that it can be passed as a single argument
in a command-line, without causing munging by a Unix or Windows shell (assuming it's either
quoted or double-quoted).
Output S is as specified as per WITH-OUTPUT.
FORMS may be a preformatted string (assumed to not contain special characters), or
a list of forms, where each form is either a preformatted string,
or a LIST that specifies a program."
  (etypecase forms
    ;; at the toplevel, a string mean "this is pre-formatted", you know what to pass to --eval
    (string
     (output-string forms s))
    (list
     (with-standard-io-syntax
       (let ((*package* (find-package :cl)))
         (with-output (s)
           (labels
               ((p (x) (princ x s)) ;; princ
                (w (x) (write x :stream s :case :downcase)) ;; write
                (c (x) ;; form that evaluates into a character
                  (p "(code-char`") (p (char-code x)) (p ")"))
                (sym (x) ;; print a symbol
                  ;; check that only good characters are used in a symbol;
                  ;; TODO: if not, go through a string (ugh!)
                  (assert (every 'non-special-symbol-character-p (package-name (symbol-package x))))
                  (assert (every 'non-special-symbol-character-p (symbol-name x)))
                  (w x))
                (s (x) ;; form that evaluates into a string
                   (if (every 'non-special-character-p x)
                       (progn ;; simple string literal
                         (p "(string`") (print-string-in-pipes x s) (p ")"))
                       (progn
                         (p "(format()(string`|~{~a~}|)`(")
                         (loop
                           :with end = (length x)
                           :for start = 0 :then (if position (1+ position) end)
                           :for morep = (< start end)
                           :for position = (and morep
                                                (position-if-not 'non-special-character-p x
                                                                 :start start))
                           :while morep
                           :do (when (or (null position) (< start position))
                                 (print-string-in-pipes (subseq x start position) s))
                               (when position
                                 (p ",") (c (char x position))))
                         (p "))"))))
                (n (x) ;; is it the easy case where no space is needed in the list?
                  (loop :for (a . d) :on x :do
                    (cond
                      ((null d) (return t))
                      ((atom d) (return nil))
                      ((or (typep a '(or null character string cons))
                           (and (typep (car d) 'list) (n (car d)))))
                      (t (return nil)))))
                (f (x) ;; top-level function to print a form without spaces, ' " \
                  (etypecase x
                    (null (p "()"))
                    (symbol (sym x))
                    (real (w x))
                    (character (p "#.") (c x))
                    (string (progn (p "#.") (s x)))
                    (cons
                     (if (n x)
                         (progn (p "(") (map () #'f x) (p ")"))
                         (progn (p "#.`") (b x))))))
                (b (x) ;; print a form that, when inside a backquote, evaluates to the form we want
                  (etypecase x
                    (null (p "()"))
                    (symbol (sym x))
                    (real (w x))
                    (character (p ",") (c x))
                    (string (progn (p ",") (s x)))
                    (cons
                     (p "(")
                     (loop :for (a . d) :on x :do
                       (b a)
                       (cond
                         ((null d)) ;; Done: d is a close paren
                         ((typep a '(or null character string cons))) ;; Done: a ends with )
                         ((atom d)
                          (p ",@") ;; NB: we rely on ,@ being the same as ., in practice
                          (typecase d
                            (character (c d))
                            (string (s d))
                            (real (w d))
                            ((or keyword (eql t)) (sym d))
                            (t (p "`") (b d))))
                         ((typep (car d) '(or list character string))) ;; Done: d starts with , or (
                         ((typep (car d) '(or real keyword boolean)) ;; insert a , before constant
                          (p ","))
                         ((typep (car d) 'symbol)
                          (p ",`")) ;; insert a ,` before variable symbol
                         (t (error "foo"))))
                     (p ")")))))
             (p "(quote(")
             (loop :for form :in forms :do
               (p "#.")
               (etypecase form
                 (string (p form)) ;; forms directly under the top-level can be preformatted, too
                 (list (f form))))
             (p "))"))))))))

(defun compose-copy-paste-string (forms &optional s)
  "Given the same input as for COMPOSE-NON-SPECIAL-STRING, output the very same FORMS
in a human-readable form that can be copy-pasted and edited onto a Common Lisp REPL."
  (etypecase forms
    (string (output-string forms s))
    (list (format s "~{~A~%~}"
                  (with-standard-io-syntax
                    (let ((*package* (find-package :cl))
                          (*print-case* :downcase))
                      (mapcar (lambda (x)
                                (typecase x (string x)
                                          (t (write-to-string x))))
                              forms)))))))
