(in-package css-lite)

;;simple usage
(css (("#foo") :bar "50px"))
"
#foo {
bar:50px;
}"

;; defining a css-variable manually

(setq my-css-var (list :inside-var "50px 30px"))
(setf (get 'my-css-var 'css-var) t)

;; using that css-variable
(css (("#foo") :bar "50px" my-css-var))
"
#foo {
bar:50px;
inside-var:50px 30px;
}"