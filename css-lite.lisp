(in-package "CSS-LITE")

;;; main interface

(defvar *css-stream* nil)

(defvar *indent-css* nil
  "Indicates if the properties of a selector should be indented or not.

There are three possible values:

* nil - The default value, and indicates that no indentation should be
  applied

* the symbol 'tab - Indicates that the properties should be indented
  using the #\Tab character

* an integer greater than 0 - Indicates how many #\Space characters
  should be used to indent the properties")

(defmacro css (&body rules)
  `(format *css-stream* "~@{~A~}" ,@(mapcan #'process-css-rule rules)))

(defmacro css-string (&body rules)
  `(with-output-to-string (*css-stream*)
    (css ,@rules)))

(defun inline-css (&rest properties)
  (format nil "~{~A~}" (process-css-properties properties t :newlines nil)))

(defun css-id-name (symbol)
  ;; This should probably be implemented using read-time conditionals,
  ;; but I can't seem to get them to work correctly. So for now let's
  ;; just directly access the `*features*' to check if parenscript has
  ;; been loaded into the image. - rolando2424
  (when (member :parenscript *features*)
    (format nil "#~(~a~)" symbol)))

(defmacro make-css-var (var-name var-val)
  `(progn 
     (setq ,var-name ,var-val)
     (setf (get ',var-name 'css-var) t)))

(defmacro make-css-func (func-name &body forms)
  `(progn
     (defun ,func-name ,@forms)
     (setf (get ',func-name 'css-func) t)))

(make-css-func comment (comment-string) (list (concatenate 'string "/*" comment-string) "*/"))


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


(defun css-func-p (val)
  (if (symbolp val)
      (get val 'css-func)
      nil))


(defun css-var-p (val)
  (if (symbolp val)
      (get val 'css-var)
      nil))

(defun css-comment-p (val)
  "Return T if `val' is the start of a CSS comment, otherwise return NIL."
  (string= val "/*" :end1 2))

(defun expand-tree (tree)
  (let ((result '()))
    (labels ((scan (item)
               (if (listp item)
                   (if (css-func-p (car item))
                       ;; this calls the function
                       (scan (eval `(,(car item) ,@(cdr item)))) 
                   (map nil #'scan item))
                   (if (css-var-p item)
                       (scan (symbol-value item))
                    (push item result)))))
      (scan tree))
    (nreverse result)))


(defun process-css-properties (properties eval-vals &key (newlines t))
  (loop for (name val) on
       (expand-tree properties)
     by #'cddr appending
       (list 
        (if newlines +newline+ "")
        (concatenate 'string 
          ;; Indent the property as specified in the variable `*indent-css*'
          (cond ((null *indent-css*) "")
            ((equal *indent-css* 'tab)
              (string #\Tab))
            ((plusp *indent-css*)
              (make-string *indent-css* :initial-element #\Space))
            ;; XXX: If the value of `*indent-css*' is invalid, this
            ;; `cond' does the same thing as if `*indent-css*' had the
            ;; value `nil'. Should it raise an error?
            )
          (to-string name)
          ;; Only add the ':' character if this isn't a comment
          (unless (css-comment-p name)
            ":"))
        (if eval-vals (to-string val) 
            `(to-string ,val))
         ;; The ';' character should only be added if this isn't a
         ;; comment
         (if (css-comment-p name)
           ""
           ";"))))

(defun process-css-rule (rule &key (parent-selectors nil))
  (let ((selectors (if parent-selectors
                       (flatten (list parent-selectors (car rule)))
                       (car rule)))
        (properties (cadr rule))
        (children-rules (cddr rule)))
    (append (list +newline+ (css-selectors-to-string selectors) " {")
            (process-css-properties properties nil)
            (list +newline+ "}" +newline+)
            (mapcan 
             #'(lambda (child-rules) 
                 (process-css-rule child-rules :parent-selectors selectors))
             children-rules))))

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
