(in-package #:css-lite-tests)

(def-suite :css-lite)

(in-suite :css-lite)

(defmacro css-is (string-value css-value)
  `(is (string-equal ,string-value
                     (eval (macroexpand ,css-value)))
       "~2&~S~2% evaluated to ~2&~S~2% which is not ~2&~S~2% to ~2&~S~2%"
       (quote ,css-value)
       (eval (macroexpand ,css-value))
       'string-equal
       ,string-value))

(def-fixture examples ()
  (unwind-protect
       (progn
         (setq my-css-var '(:margin "50px 30px"))
         (setf (get 'my-css-var 'css-lite::css-var) t)
         (make-css-var my-favorite-border-var '(:border "1px solid red"))
         (make-css-func foo-func2 (avar) (list avar avar))
         (&body))
    (makunbound 'my-css-var)
    (makunbound 'my-favorite-border-var)
    (fmakunbound 'foo-func2)))

(def-test simple-usage ()
  (css-is "
#foo {
height:50px;
}
"
          '(css
            (("#foo")
             (:height "50px")))))

(def-test css-variables-usage (:fixture examples)
  (css-is "
#foo {
height:50px;
margin:50px 30px;
}
"
          '(css
             (("#foo")
              (:height "50px" my-css-var)))))

(def-test css-make-css-var-macro (:fixture examples)
  (css-is "
#foo {
height:50px;
margin:50px 30px;
border:1px solid red;
}
"
          '(css
             (("#foo")
              (:height "50px" my-css-var my-favorite-border-var)))))

(def-test css-make-css-var-macro (:fixture examples)
  (css-is "
#foo {
length:50px;
margin:50px 30px;
border:1px solid red;
}

#foo li {
width:50px;
float:left;
margin:50px 30px;
border:1px solid red;
}
" '(css
     (("#foo")
      (:length "50px" my-css-var my-favorite-border-var)
      (("li")
       (:width "50px"
        :float "left"
        my-css-var my-favorite-border-var))))))


(def-test css-function (:fixture examples)
  (css-is "
#foo {
should-be-repeated:should-be-repeated;
height:50px;
margin:50px 30px;
border:1px solid red;
}

#foo li {
width:50px;
float:left;
margin:50px 30px;
border:1px solid red;
}
"
          '(css
            (("#foo")
             ((foo-func2 "should-be-repeated")
              :height "50px"
              my-css-var my-favorite-border-var)
             (("li")
              (:width "50px"
               :float "left"
               my-css-var my-favorite-border-var))))))

(def-test css-comment (:fixture examples)
  (css-is "
#foo {
/*a comment*/
should-be-repeated:should-be-repeated;
height:50px;
margin:50px 30px;
border:1px solid red;
}

#foo li {
width:50px;
margin:50px 30px;
border:1px solid red;
}
"
          '(css
             (("#foo")
              ((comment "a comment" )
               (foo-func2 "should-be-repeated")
               :height "50px"
               my-css-var my-favorite-border-var)
              (("li")
               (:width "50px" my-css-var my-favorite-border-var))))))


(def-test css-indent-tab (:fixture examples)
  (let  ((*indent-css* 'css-lite::tab))
    (css-is "
#foo {
	/*a comment*/
	should-be-repeated:should-be-repeated;
	height:50px;
	margin:50px 30px;
	border:1px solid red;
}

#foo li {
	width:50px;
	margin:50px 30px;
	border:1px solid red;
}
"
            '(css
               (("#foo")
                ((comment "a comment" )
                 (foo-func2 "should-be-repeated")
                 :height "50px" my-css-var my-favorite-border-var)
                (("li")
                 (:width "50px" my-css-var my-favorite-border-var)))))))

(def-test css-indent-spaces (:fixture examples)
  (let  ((*indent-css* 2))
    (css-is "
#foo {
  /*a comment*/
  should-be-repeated:should-be-repeated;
  height:50px;
  margin:50px 30px;
  border:1px solid red;
}

#foo li {
  width:50px;
  margin:50px 30px;
  border:1px solid red;
}
"
            '(css
               (("#foo")
                ((comment "a comment" )
                 (foo-func2 "should-be-repeated")
                 :height "50px" my-css-var my-favorite-border-var)
                (("li")
                 (:width "50px" my-css-var my-favorite-border-var)))))))
