(setf ccl:*DEFAULT-EXTERNAL-FORMAT*
      (ccl:make-external-format :character-encoding :utf-8 :line-termination :unix)
      ccl:*default-file-character-encoding* :utf-8
      ccl:*default-socket-character-encoding* :utf-8)

;;; load babel2
(load "/home/<USER>/Babel2/libraries/asdf")
(load "/home/<USER>/Babel2/init-babel")
