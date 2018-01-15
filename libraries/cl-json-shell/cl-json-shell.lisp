(in-package :cl-json-shell)

(export '(encode-json-to-string-for-shell))

(defun encode-json-to-string-for-shell (object)
  "Adds extra single quotes for lispworks. Don't ask me (Paul) why it's needed."
  #+lispworks  (utils:string-append "'" (utils:string-replace (cl-json:encode-json-to-string object) "'"  "'\\''") "'")
  #-lispworks (cl-json:encode-json-to-string object))

