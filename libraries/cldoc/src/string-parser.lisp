;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUDG; -*-
;;; $Id: string-parser.lisp,v 1.1.1.1 2005/11/18 14:52:18 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: Common Lisp Universal Documentation Generator: doc string parser
;;;   Created: 2005 10 23 23:30
;;;    Author: Iban Hatchondo <hatchond@yahoo.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Iban Hatchondo

;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; Documentation string DWIM parser utilities

(in-package :cludg)

;;; Protocol & definitions.

(defconstant +default-link-delimiters+ '(#\{ #\}))
(defconstant +default-section-prefix+ "* ")
(defconstant +default-code-prefix+ ";;; ")
(defconstant +default-section-names+
  '("Arguments and Values:" "Side Effects:" "Affected By:"
    "Exceptional Situations:" "See Also:" "Notes:"))

(defclass doctree ()
  ((tree :initform (make-tree :doc) :type array)
   (bulleted-list-opened-p :initform nil :type boolean)
   (bulleted-list-level :initform 0 :type fixnum)
   (last-line :initform nil :type (or null string))
   (section-prefix
     :initform +default-section-prefix+
     :type string
     :initarg :section-prefix
     :reader section-prefix)
   (section-names 
    :initform +default-section-names+
    :type list
    :initarg :section-names
    :reader section-names)
   (link-delimiters
     :initform +default-link-delimiters+
     :type list
     :initarg :link-delimiters)
   (code-prefix
     :initform +default-code-prefix+
     :type string
     :initarg :code-prefix
     :reader code-prefix)
   (item-prefix-maker
     :initform #'make-item-prefix
     :type function
     :initarg :item-prefix-maker))
  (:documentation "This class will be used to represent the doc string
   structure. Context such as paragraph and bulleted list will be
   repesented as vector block. This is used to retreive the context
   the documentation writer has indicated by its 'Do What I Mean' block.
    It recognizes both indent and empty-line paragraph breaks, bulleted lists,
   code sample, hyper link and sections (like in the Hyperspec).

   For bulleted lists the grammar can be specified using the
  :item-prefix-maker option of the driver. To end itemized list, just add
   a blank line after the last item. The depth of bulleted lists is not
   constrained, but if you start sub bulleted list then a blank line will
   end the current one and all parents at the same time. Otherwise said,
   like here, no other paragraph will be permitted in an item after its
   sub list items.
    The sublists item designator will obey to the following grammar unless
   you have specified your own grammar (see the :item-prefix-maker option):
     - (- ) is the first level of list item prefix.
     - (-- ) is the second level of list item prefix.
     - (--- ) is the third level of list item prefix and so on.

   Use the following options to customize the parser:
   - :item-prefix-maker (function): a designator for a function of one 
     argument. Its argument will be an (integer 1 *) that represents the 
     depth of the list. The return value is the corresponding string prefix
     designator for bulleted list (sublist) items of the specified depth.
   - :code-prefix (string): a string that designates a prefix for code snipet
     insertion in the documentation string. It must prefix all lines of code
     in the documentation string.
     The default value is: {defconstant +default-code-prefix+} .
     For exemple:
     ;;; (defun cludg-sample (bar)
     ;;;   \"How to prefix code snipet in the documentation string:
     ;;;    ;;; (setf *print-case* :downcase)
     ;;;    You are, of course, not limited to one line snipet.\"
     ;;;   (do-nothing))
   - :section-prefix (string): a string that will be used to determine if a 
     section must be started or not if found at the beginning (ignoring left
     whitespaces) of the line. The default value is: 
     {defconstant +default-section-prefix+} .
   - :section-names (string list): a list of string indicating the section
     names. This must be used in conjonction with the section-prefix.
     For instance start a line as follows: '* See Also:'. Default value is:
     {defconstant +default-section-names+} .
   - :link-delimiters (list of two character): a list of two characters that
     indicates the link opening and closing characters. Default value is:
     {defconstant +default-link-delimiters+} . Link grammar:
     [opening-char(URL | defun | defclass | ...)closing-char]. If the hyper
     link can be resolved."))

(defgeneric doctree-tree (doctree)
  (:documentation "Returns the tree that represent this doctree instance."))

(defgeneric link-delimiters (doctree)
  (:documentation "Returns as a multiple value the left and right
   characters that delimits a hyper link in a documentation string."))

(defgeneric link-found-p (doctree word words)
  (:documentation "Returns T and the length (in words) if any hyper link is
   found. An hyper link will be found if the first character of word is equal
   to the specified doctree link-delimiters open-char and if any word of the
   (word . words) ends with the specified doctree link-delimiters
   closing-char."))

(defgeneric doc-section-p (doctree string)
  (:documentation "If the doctre section prefix delimiter is a prefix of the
   given string then returns as a multiple value the string without section
   prefix delimiter, and the section name."))

(defgeneric bulleted-list (doctree level)
  (:documentation "Returns the bulleted list of given level."))

(defgeneric last-list-item (doctree level)
  (:documentation "Returns the last item of the bulleted list of given
   level."))

(defgeneric paragraph-handle-line (doctree string &optional subtree)
  (:documentation  "This method is typically invoked within add-to-paragraph.
   It insert the given string in the doctree tree or in the subtree if given.
    This the place for word recognition ; with the default implemention lisp
   keyword will be recognized and added within a keyword subtree block."))

(defgeneric add-section (doctree section-name string &optional tree)
  (:documentation "Insert a section of title section-name in the specified
  doctree-tree (or subtree if specified). Any opened paragraph will be closed
  before. Then if the result of trimming the section-name of string result in
  a non empty string then the remaining substring will be added in a newly
  opened paragraph."))

(defgeneric add-to-paragraph (doctree string &optional subtree)
  (:documentation  "Insert the given string in the last paragraph of the
   doctree tree or in the subtree if given. A paragraph will opened when
   needed before inserting the string. New paragraph will be opened under
   the following circumstances:
    - If the last element in the tree is not a paragraph.
    - If given string is empty and the previous paragraph is not empty.
    - If the last line of the paragraph ends with #\. and has less left
      #\Space characters than the given one."))

(defgeneric add-to-code-block (doctree string &optional subtree)
  (:documentation "Insert the given string, after removing its code-prefix,
  in the last code-block of the specified doctree (or subtree if specified).
  A new code-block will be opened in the doctree, or in the subtree, if the
  last block is not a code-block."))

(defgeneric add-to-bulleted-list-item (doctree string)
  (:documentation "Adds a string to the latest item of the latest most inner
   bulleted list."))

(defgeneric add-to-bulleted-list (doctree string level)
  (:documentation "Adds an item to the latest most inner bulleted list of
   the given level. If no such level of bulleted list exists, then it will
   be created."))

(defgeneric handle-string (doctree string)
  (:documentation "Adds a string into the doctree. This method will be  
   responsible for deciding in which subtree the given string should be
   added."))

(defgeneric create-doctree-from-string (type strings &key &allow-other-keys)
  (:documentation "Returns the document tree represented by the given
   strings when parsed with some Do What I Mean functions.")
  (:method ((type (eql 'doctree)) strings
	    &key (item-prefix-maker #'make-item-prefix)
	         (code-prefix +default-code-prefix+)
	         (section-prefix +default-section-prefix+)
	         (section-names +default-section-names+)
	         (link-delimiters +default-link-delimiters+)
	    &allow-other-keys)
    (let ((dtree (make-instance type
		     :link-delimiters link-delimiters
		     :section-prefix section-prefix
		     :section-names section-names
		     :item-prefix-maker item-prefix-maker
		     :code-prefix code-prefix)))
      (loop for string in strings do (handle-string dtree string))
      (doctree-tree dtree))))

;;; Misc functions.

(defun make-tree (tag)
  (let ((tree (make-array 10 :adjustable t :fill-pointer 0 :element-type t)))
    (vector-push-extend tag tree)
    tree))

(defun tree-add (element tree)
  "Adds the specified element in the given tree."
  (vector-push-extend element tree) tree)

(defun make-item-prefix (depth)
  "Returns the desired list item designator according to te given depth. 
   The depth is an integer greater than zero - aka: (integer 1 *)."
  (declare (type (integer 1 *) depth))
  (concatenate 'string (make-string depth :initial-element #\-) " "))

(defun trim-left-spaces (string)
  "Returns a substring of string, with all Tab and Space characters stripped
   off the beginning."
  (string-left-trim '(#\Tab #\Space) string))

(defun trim-prefix (prefix string &key (replace-prefix t))
  "Returns a new string that does not contain prefix anymore. Left white spaces
   will be ignored but kept. Prefix will be replace by as many space characters
   than its length if replace-prefix is T."
  (let ((start (or (position (char prefix 0) string :test #'char=) 0)))
    (concatenate 'string
		 (subseq string 0 start)
		 (if replace-prefix
		     (make-string (length prefix) :initial-element #\Space)
		     "")
		 (subseq string (+ start (length prefix))))))

(defun starts-with (string prefix &optional ignore-left-whitespace-p)
  "Returns T if the designed string starts with the desired string prefix."
  (when ignore-left-whitespace-p (setf string (trim-left-spaces string)))
  (unless (< (length string) (length prefix))
    (loop for i from 0 below (length prefix)
	  unless (char= (char string i) (char prefix i))
	  do (return nil) finally (return T))))

(defun start-para-p (prev string)
  "Returns T if the given string correspond to the beginning of a paragraph
   in regards to the previous one. Otherwise said: If prev the previous one
   ends with #\. and string the next one has more left white space than the
   previous one. Otherwise NIL is returned."
  (flet ((last-char (string) (char string (1- (length string)))))
    (when (and (string/= prev "") (char= #\. (last-char prev)))
      ;; when previous line ends with a '.'
      (let ((n1 (position #\Space prev :test #'char/=))
	    (n2 (position #\Space string :test #'char/=)))
	;; returns T if nb begining space char n2 is greater than n1
	(and n2 n1 (> n2 n1))))))

(defmacro with-tree-loop ((var tree) &body body)
  "Iterates over tree elements. var, a symbol, will be bound at each iteration
   to the next element of the tree."
  (let ((i (gensym)))
    `(loop for ,i from 1 below (length ,tree)
           for ,var = (aref ,tree ,i) do ,@body)))

(defun tree-tag (tree)
  "Returns the tag of the specified tree."
  (aref tree 0))

(defun tree-p (object)
  "Returns T if the specified object is a doctree tree."
  (arrayp object))

(defun block-list-p (tree)
  "Returns T if the specified tree represent a list."
  (and (tree-p tree) (eq :ul (tree-tag tree))))

(defun item-p (tree)
  "Returns T if the specified tree represent a item of a list."
  (and (tree-p tree) (eq :li (tree-tag tree))))

(defun paragraph-p (tree)
  "Returns T if the specified tree represent a paragraph."
  (and (tree-p tree) (eq :p (tree-tag tree))))

(defun code-block-p (tree)
  "Returns T if the specified tree represent a block of code."
  (and (tree-p tree) (eq :pre (tree-tag tree))))

(defun string-bulleted-item-p (string level item-prefix-maker)
  "Returns T if the given string starts with the bulleted list prefix
   of the specified level."
  (declare (type string string))
  (declare (type fixnum level))
  (declare (type function item-prefix-maker))
  (starts-with string (funcall item-prefix-maker level) t))

(defun close-paragraph (doctree)
  (setf (slot-value doctree 'last-line) nil))  

(defun split (string &optional (word-separator '(#\Space #\Tab)))
  (flet ((word-separator-p (char) (find char word-separator)))
    (nreverse
     (let ((list nil) (start 0) end)
       (loop
        (setf end (position-if #'word-separator-p string :start start))
        (push (subseq string start end) list)
        (unless end (return list))
        (setf start (1+ end)))))))

;;; Protocol Implemention.

(defparameter *newline* (string #\Newline))

(declaim (inline tree-add tree-tag tree-p paragraph-p block-list-p item-p))

(defmethod doctree-tree ((doctree doctree))
  (slot-value doctree 'tree))

(defmethod link-delimiters ((doctree doctree))
  (values-list (slot-value doctree 'link-delimiters)))

(defmethod link-found-p ((doctree doctree) word words)
  (multiple-value-bind (open-char closing-char) (link-delimiters doctree)
    (flet ((close-mark-found-p (str)
	     (char= closing-char (char str (1- (length str))))))
      (cond ((char/= open-char (char word 0)) (values NIL 0))
	    ((close-mark-found-p word) (values T 1))
	    (t (loop for str in words and nb-items from 2 ; word + the rest !
		     when (char= closing-char (char str (1- (length str))))
		     do (return-from link-found-p (values T nb-items))))))))

(defmethod doc-section-p ((doctree doctree) string)
  (when (starts-with string (section-prefix doctree) t)
    (loop with substr = (trim-prefix (section-prefix doctree) string)
          for section in (section-names doctree)
          when (starts-with substr section t)
          do (return-from doc-section-p (values substr section)))))

(defmethod add-section
    ((doctree doctree) section string  &optional (tree (doctree-tree doctree)))
  (let ((substr (trim-prefix section string :replace-prefix nil)))
    (with-slots (bulleted-list-opened-p bulleted-list-level) doctree
      (setf bulleted-list-opened-p (close-paragraph doctree)
            bulleted-list-level 0)
      (tree-add (tree-add section (make-tree :h4)) tree)
      (unless (string= "" (trim-left-spaces substr))
        (add-to-paragraph doctree substr)))))

(defmethod add-to-code-block
    ((doctree doctree) string &optional (tree (doctree-tree doctree)))
  (let ((code-block (aref tree (1- (length tree)))))
    ;; Open a code block if the last element in the tree is not a code block.
    (unless (code-block-p code-block)
      (tree-add (setf code-block (make-tree :pre)) tree)
      (close-paragraph doctree))
    (when (> (length code-block) 1)
      (tree-add *newline* code-block))
    ;; Remove left white space characters before prefix.
    (tree-add
        (trim-prefix (code-prefix doctree) (trim-left-spaces string))
	code-block)))

(defmethod paragraph-handle-line
    ((doctree doctree) string &optional (tree (doctree-tree doctree)))
  (loop with words = (split string) and link-length = 0 and link-found-p = nil
	for word = (pop words) for wl = (length word) while word
	if (> wl 1)
	 if (and (char= #\: (char word 0)) (char/= #\: (char word (1- wl))))
	 ;; keyword found: add a :keyword block in tree
	 do (tree-add (tree-add word (make-tree :keyword)) tree)
	 else if (multiple-value-setq (link-found-p link-length)
		   (link-found-p doctree word words))
	 ;; hyper link found => add an :hyper-link block in tree
	 do (loop with item = (make-tree :hyper-link)
		  for i from 1 to link-length
		  for part = (subseq word 1) then (pop words)
		  if (< i link-length) do (tree-add part item)
		  else do (tree-add (subseq part 0 (1- (length part))) item)
		  finally (tree-add item tree))
	 else do (tree-add word tree)
        else if (> wl 0) do (tree-add word tree))
  (tree-add *newline* tree))

(defmethod add-to-paragraph
    ((doctree doctree) string &optional (tree (doctree-tree doctree)))
  (with-slots (last-line) doctree
    ;; If string is belongs to a block of code then add-to-code-block.
    (when (starts-with string (code-prefix doctree) t)
      (add-to-code-block doctree string tree)
      (return-from add-to-paragraph (close-paragraph doctree)))
    (let ((para (aref tree (1- (length tree)))))
      ;; Open paragraph if the last element in the tree is not a paragraph.
      (unless (paragraph-p para)
	(tree-add (setf para (make-tree :p)) tree))
      ;; If string is the empty string return. But, if last paragraph is not
      ;; empty open a new one before return.
      (when (string= "" string)
	(when (> (length para) 1) (tree-add (make-tree :p) tree))
	(return-from add-to-paragraph (close-paragraph doctree)))
      ;; Open a new paragraph if the last line of the current one ends with '.'
      ;; and string starts with more #\Space than previous one (according to
      ;; start-para-p predicate).
      ;; But skip paragraph creation if current one has only one line.
      (when (and (stringp last-line) (start-para-p last-line string))
	(tree-add (setf para (make-tree :p)) tree))
      ;; Finally add the string and update last-line.
      (paragraph-handle-line doctree string para)
      (setf last-line string))))

(defmethod bulleted-list ((doctree doctree) level)
  (with-slots (tree bulleted-list-level) doctree
    (when (zerop bulleted-list-level)
      (tree-add (make-tree :ul) tree))
    (loop with btree = tree
          for depth from 0 below level
	  for length = (length btree)
	  do (when (item-p (aref btree (1- length)))
	       (setf btree (aref btree (1- length)))
	       (setf length (length btree)))
	     (unless (block-list-p (aref btree (1- length)))
	       (tree-add (make-tree :ul) btree)
	       (setf btree (aref btree length))
	       (loop-finish))
	     (setf btree (aref btree (1- (length btree))))
	  finally (when (not (block-list-p btree))
		    (let ((foo (make-tree :ul)))
		      (tree-add foo btree)
		      (setf btree foo)))
	          (return btree))))

(defmethod last-list-item ((doctree doctree) level)
  (let ((tree (bulleted-list doctree level)))
    (aref tree (1- (length tree)))))

(defmethod add-to-bulleted-list ((doctree doctree) string level)
  (with-slots (bulleted-list-opened-p bulleted-list-level item-prefix-maker)
      doctree
    (let ((btree (bulleted-list doctree level))
	  (prefix (funcall item-prefix-maker level))
	  (item (make-tree :li)))
      ;; End last paragraph.
      (close-paragraph doctree)
      ;; Handle the first item.
      (add-to-paragraph doctree (trim-prefix prefix string) item)
      (tree-add item btree)
      (setf bulleted-list-level level)
      (setf bulleted-list-opened-p t))))

(defmethod add-to-bulleted-list-item ((doctree doctree) string)
  (with-slots (bulleted-list-opened-p bulleted-list-level) doctree
    (if (string= string "")
	(setf bulleted-list-opened-p (close-paragraph doctree)
	      bulleted-list-level 0)
	(let ((last-item (last-list-item doctree bulleted-list-level)))
	  (add-to-paragraph doctree string last-item)))))

(defmethod handle-string ((doctree doctree) string)
  (with-slots
	(bulleted-list-opened-p bulleted-list-level item-prefix-maker tree)
      doctree
    ;; Handle HyperSpec like sections if necessary.
    (multiple-value-bind (str section-name) (doc-section-p doctree string)
      (when section-name
	(add-section doctree section-name str)
	(return-from handle-string nil)))
    ;; Check if it is a bulleted item then add it to its list.
    (loop for level from (1+ bulleted-list-level) downto 1
	  if (string-bulleted-item-p string level item-prefix-maker)
	  do (add-to-bulleted-list doctree string level)
	     (return-from handle-string nil))
    ;; Else add to last bulleted item or to current para.
    (if bulleted-list-opened-p
	(add-to-bulleted-list-item doctree string)
	;; Add to last paragraph.
	(add-to-paragraph doctree string))))
