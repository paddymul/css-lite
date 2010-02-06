(in-package "CSS-LITE")

;;; main interface

(defvar *css-stream* nil)

(defmacro css (&body rules)
  `(format *css-stream* "~@{~A~}" ,@(mapcan #'process-css-rule rules)))

#+parenscript (ps:defpsmacro css (&body rules)
                (cons 'ps:+ (ps::concat-constant-strings (mapcan #'process-css-rule rules))))

(defmacro css-string (&body rules)
  `(with-output-to-string (*css-stream*)
    (css ,@rules)))

(defun inline-css (&rest properties)
  (format nil "~{~A~}" (process-css-properties properties t :newlines nil)))

#+parenscript (ps:defpsmacro inline-css (&rest properties)
                (cons 'ps:+ (ps::concat-constant-strings (process-css-properties properties nil :newlines nil))))

;;; handy utility functions for writing css

(defun px (val)
  (format nil "~apx" val))

#+parenscript (ps:defpsmacro px (val)
                `(ps:+ ,val "px"))

(defun % (val)
  (format nil "~a%" val))

#+parenscript (ps:defpsmacro % (val)
                `(ps:+ ,val "%"))

(defun pt (val)
  (format nil "~apt" val))

#+parenscript (ps:defpsmacro pt (val)
                `(ps:+ ,val "pt"))

;;; implementation

(defun selector-to-string (selector)
  (handler-case 
      (if (listp selector)
          (destructuring-bind (specifier element)
              selector
            (ecase specifier
              (:hover (format nil "~a:hover" (selector-to-string element)))
              (:id (css-id-name element))))
          (cond ((and (symbolp selector) (not (symbol-package selector))) (css-id-name selector))
                ((eql :and selector) ",")
                (t (to-string selector))))
    (error () (error "~s isn't a valid CSS selector." selector))))

(defun css-selectors-to-string (selectors)
  (reduce (lambda (s1 s2) (concatenate 'string s1 " " s2)) (mapcar #'selector-to-string selectors)))

(defvar +newline+ (format nil "~%"))

(defun process-css-properties (properties eval-vals &key (newlines t))
  (loop for (name val) on properties by #'cddr appending
       (list (if newlines +newline+ "") (to-string name) ":" (if eval-vals (to-string val) `(to-string ,val)) ";")))

(defun process-css-rule (rule)
  (append (list +newline+ (css-selectors-to-string (car rule)) " {")
          (process-css-properties (cdr rule) nil)
          (list +newline+ "}")))

#-parenscript (defun css-id-name (symbol)
                (format nil "#~(~a~)" symbol))

#+parenscript (defun css-id-name (symbol)
                (format nil "#~a" (ps:symbol-to-js-string symbol)))

(defun to-string (x)
  (cond ((stringp x) x)
	((symbolp x) (string-downcase (symbol-name x)))
        ((listp x) (apply #'concatenate 'string
                          (loop for (val . rest) on x
                                with comma = ", "
                                unless rest do (setf comma "")
                                collect (if (stringp val)
                                            (format nil "'~a'" val)
                                            (to-string val))
                                collect comma)))
	(t (princ-to-string x))))

#+parenscript (ps:defpsmacro to-string (x)
                (if (and (listp x) (eql 'quote (car x)))
                    (ps:symbol-to-js-string (second x))
                    x))
