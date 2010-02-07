(in-package css-lite)
;;; handy utility functions for writing css

(defun px (val)
  (format nil "~apx" val))

(defun % (val)
  (format nil "~a%" val))

(defun pt (val)
  (format nil "~apt" val))
