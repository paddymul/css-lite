(in-package "CSS-LITE")

;; currently unused
(defun flatten (tree)
  (let ((result '()))
    (labels ((scan (item)
               (if (listp item)
                 (map nil #'scan item)
                 (push item result))))
      (scan tree))
    (nreverse result)))

