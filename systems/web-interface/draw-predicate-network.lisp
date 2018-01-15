
(in-package :web-interface)

(export '(draw-predicate-network predicate-network-with-wiki-links->svg predicate-network->svg predicate-network-with-wikilinks->s-dot))

(defun argument-p (arg predicate-network)
  "Checks whether arg occurs as a third element of one of the
predicates in the network. This is to prevent to get edges between
types (and not arguments)."
  (find arg (mapcar #'third predicate-network) :test #'equalp))

(defun predicate-network->s-dot (predicate-network &key draw-arrows (topic nil) (only-variables t)
                                                   (extensional-meanings nil)) 
  (let ((predicate-network-with-line-numbers 
         (loop for x in predicate-network for i from 1 collect (cons i x)))
        (graph '(((s-dot::ranksep "0.3") (s-dot::nodesep "0.5")
                  (s-dot::margin "0")) s-dot::graph)))
    (loop
     for x in predicate-network-with-line-numbers
     for pos = (car x)
     for expression = (cdr x)
     do (push ;; make a record node
              `(s-dot::record  
                ((s-dot::color "#ffffff")
                 (s-dot::fontsize "8.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01"))
                ;; the predicate
                (s-dot::node ((s-dot::id ,(make-dot-id pos (car expression)))
                              (s-dot::label ,(mkstr "(" (escape-dot-label 
                                                         (car expression))))))
                ;; the arguments. 
                ,@(loop for arg in (cdr expression)
                        for i from 1
                        collect
                        `(s-dot::node ((s-dot::id ,(make-dot-id pos arg))
                                       ,(if (find arg topic)
                                          `(s-dot::label 
                                            ,(mkstr "*" (escape-dot-label arg)
                                                    (if (= i (length (cdr expression)))
                                                      "*)" "*")))
                                          `(s-dot::label 
                                            ,(mkstr (escape-dot-label arg)
                                                    (if (= i (length (cdr expression)))
                                                      ")" ""))))))))
              graph))
    (loop
     ;;make the edges.
     while predicate-network-with-line-numbers
     for x = (car predicate-network-with-line-numbers)
     for pos = (car x)
     for expression = (cdr x)
     do (loop
         with variable-pos = 0
         for arg in (cdr expression)
         for arg-index from 2
         ;; when it is a variable
         when (or (not only-variables)
                  (and (symbolp arg) (equal (char (symbol-name arg) 0) #\?)))
         do
         (incf variable-pos) 
         (loop for target in (remove x predicate-network-with-line-numbers)
               when (if (and extensional-meanings ;;we use extensional-meanings 
                             (= arg-index 2)) ;; and this is the second element (i.e. first argument)
                      (when (argument-p arg predicate-network) ;;then check if it is NOT a type
                        (find arg target :test #'equal))
                      (find arg target :test #'equal))
               do (push `(s-dot::edge ((s-dot::from ,(make-dot-id pos arg))
                                       (s-dot::to ,(make-dot-id (car target) arg))
                                       ,@(if draw-arrows
                                           `((s-dot::dir ,(if (= variable-pos 1)
                                                            "forward" "back"))
                                             (s-dot::arrowsize "0.5"))
                                           '((s-dot::dir "none")))))
                        graph)
               ;; add a second invisible edge into the other direction
               ;; in order to avoid hierarchy constrains
               (push `(s-dot::edge ((s-dot::to ,(make-dot-id pos arg))
                                    (s-dot::from ,(make-dot-id (car target) arg))
                                    (s-dot::style "invis")))
                     graph)
               (return)))
     ;; this makes sure that we only add new edges
     (setf predicate-network-with-line-numbers
           (cdr predicate-network-with-line-numbers)))
    (reverse graph)))

(defun url-exists-p (url)
  "Checks if an URL exists and returns true if it did (when no 404
error was found). It uses curl to check its availability."
  (when url
    (let* ((curl-response
            (first
             (utils:exec-and-return "curl" "-s" "--head" (format nil "~a" url) "| head -n 1" )))
           (response-as-list-of-strings (split-sequence::split-sequence #\Space curl-response)))
      (or (find "301" response-as-list-of-strings :test #'string=)
          (find "200" response-as-list-of-strings :test #'string=)))))


(defun predicate-network-with-wikilinks->s-dot (predicate-network &key draw-arrows (topic nil) (only-variables t)
                                                   (extensional-meanings nil) (predicate-location #'second))
  "Draw a predicate network with wikilinks. For the English grammar,
the predicate location is by default set to the second element in the
list."
  (let ((predicate-network-with-line-numbers 
         (loop for x in predicate-network for i from 1 collect (cons i x)))
        (graph '(((s-dot::ranksep "0.3") (s-dot::nodesep "0.5") (s-dot::tooltip "Resulting Meaning")
                  (s-dot::margin "0")) s-dot::graph)))
    (loop
     for x in predicate-network-with-line-numbers
     for pos = (car x)
     for expression = (cdr x)
     for href-key-word = (apply predicate-location (list expression))
     for predicate-href = (unless (variable-p href-key-word)
                            (format nil "https://en.wikipedia.org/wiki/~@(~a~)" (string-replace href-key-word "-" "_")))
     for existing-url = (when (url-exists-p predicate-href)
                          predicate-href)
     do (push ;; make a record node
              `(s-dot::record  
                ((s-dot::color "#ffffff")
                 (s-dot::fontcolor ,(if existing-url
                                      "#0000FF" "#000000"))
                 (s-dot::fontsize "8.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")
                 (s-dot::target "_graphiz") ;;to open linked predicates in new tab
                 ,@(if existing-url
                      `((s-dot::href ,existing-url)
                        (s-dot::tooltip ,existing-url))
                      '((s-dot::tooltip ""))))
                ;; the predicate
                (s-dot::node ((s-dot::id ,(make-dot-id pos (car expression)))
                              (s-dot::label ,(mkstr "(" (escape-dot-label 
                                                         (car expression))))))
                ;; the arguments. 
                ,@(loop for arg in (cdr expression)
                        for i from 1
                        collect
                        `(s-dot::node ((s-dot::id ,(make-dot-id pos arg))
                                       ,(if (find arg topic)
                                          `(s-dot::label 
                                            ,(mkstr "*" (escape-dot-label arg)
                                                    (if (= i (length (cdr expression)))
                                                      "*)" "*")))
                                          `(s-dot::label 
                                            ,(mkstr (escape-dot-label arg)
                                                    (if (= i (length (cdr expression)))
                                                      ")" ""))))))))
              graph))
    (loop
     ;;make the edges.
     while predicate-network-with-line-numbers
     for x = (car predicate-network-with-line-numbers)
     for pos = (car x)
     for expression = (cdr x)
     do (loop
         with variable-pos = 0
         for arg in (cdr expression)
         for arg-index from 2
         ;; when it is a variable
         when (or (not only-variables)
                  (and (symbolp arg) (equal (char (symbol-name arg) 0) #\?)))
         do
         (incf variable-pos) 
         (loop for target in (remove x predicate-network-with-line-numbers)
               when (if (and extensional-meanings ;;we use extensional-meanings 
                             (= arg-index 2)) ;; and this is the second element (i.e. first argument)
                      (when (wi::argument-p arg predicate-network) ;;then check if it is NOT a type
                        (find arg target :test #'equal))
                      (find arg target :test #'equal))
               do (push `(s-dot::edge ((s-dot::from ,(make-dot-id pos arg))
                                       (s-dot::to ,(make-dot-id (car target) arg))
                                       (s-dot::tooltip "->")
                                       ,@(if draw-arrows
                                           `((s-dot::dir ,(if (= variable-pos 1)
                                                            "forward" "back"))
                                             (s-dot::arrowsize "0.5"))
                                           '((s-dot::dir "none")))))
                        graph)
               ;; add a second invisible edge into the other direction
               ;; in order to avoid hierarchy constrains
               (push `(s-dot::edge ((s-dot::to ,(make-dot-id pos arg))
                                    (s-dot::from ,(make-dot-id (car target) arg))
                                    (s-dot::style "invis")))
                     graph)
               (return)))
     ;; this makes sure that we only add new edges
     (setf predicate-network-with-line-numbers
           (cdr predicate-network-with-line-numbers)))
    (reverse graph)))

     
;; ############################################################################
;; draw-predicate-network
;; ----------------------------------------------------------------------------

(defun draw-predicate-network (predicate-network &key path (open nil) (format "png") draw-arrows)
  "Uses s-dot to draw a predicate network (i.e., an irl-program).
   Returns the pathname of the generated graphic. When :open t,
   then it tries to open the predicate-network."
  (s-dot->image (predicate-network->s-dot predicate-network :draw-arrows draw-arrows)
                :path path :open open :format format))

;; ############################################################################
;; predicate-network->svg
;; ----------------------------------------------------------------------------

(define-css 'predicate-network-svg "
div.predicate-network-svg { overflow:hidden;}
div.predicate-network-svg > svg { margin-left:-8px;margin-right:-8px;margin-top:-3px;margin-bottom:-8px; }
")

(defun predicate-network->svg (predicate-network &key draw-arrows (topic nil) (only-variables t)
                                                 (extensional-meanings nil))
  "renders predicate networks, i.e., irl programs,
   into an svg that can be readily used in the web interface"
  `((div :class "predicate-network-svg")
    ,(s-dot->svg (predicate-network->s-dot predicate-network :draw-arrows draw-arrows :topic topic :only-variables only-variables :extensional-meanings extensional-meanings))))


(defun predicate-network-with-wiki-links->svg (predicate-network &key draw-arrows (topic nil) (only-variables t)
                                                 (extensional-meanings nil))
  "renders predicate networks, i.e., irl programs,
   into an svg that can be readily used in the web interface"
  `((div :class "predicate-network-svg")
    ,(s-dot->svg (predicate-network-with-wikilinks->s-dot predicate-network :draw-arrows draw-arrows :topic topic :only-variables only-variables :extensional-meanings extensional-meanings))))

