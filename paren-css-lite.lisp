
#+parenscript (ps:defpsmacro px (val)
                `(ps:+ ,val "px"))

#+parenscript (ps:defpsmacro % (val)
                `(ps:+ ,val "%"))

#+parenscript (ps:defpsmacro pt (val)
                `(ps:+ ,val "pt"))

#+parenscript (ps:defpsmacro css (&body rules)
                (cons 'ps:+ (ps::concat-constant-strings (mapcan #'css-lite::process-css-rule rules))))


#+parenscript (ps:defpsmacro inline-css (&rest properties)
                (cons 'ps:+ (ps::concat-constant-strings (css-lite::process-css-properties properties nil :newlines nil))))


#+parenscript (ps:defpsmacro to-string (x)
                (if (and (listp x) (eql 'quote (car x)))
                    (ps:symbol-to-js-string (second x))
                    x))


#+parenscript (defun css-id-name (symbol)
                (format nil "#~a" (ps:symbol-to-js-string symbol)))
