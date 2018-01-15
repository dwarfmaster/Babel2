#|
Copyright (C) 2007, 2008  David Owen <dsowen@fugue88.ws>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser Public License for more details.

You should have received a copy of the GNU Lesser Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(require '#:cl-ppcre)
(require '#:dso-lex)

(use-package '(#:cl-ppcre #:dso-lex))



(defun snip (s) (subseq s 1 (1- (length s))))

(defun un-squote (s) (regex-replace-all "''" (snip s) "'"))

(defun un-dquote (s) (regex-replace-all "\"\"" (snip s) "\""))

(deflexer scan-csv (:priority-only t)
  ("," comma)
  ("[^\"',]+" value)
  ("'(?:[^']|'')*'" value un-squote)
  ("\"(?:[^\"]|\"\")*\"" value un-dquote))



(lex-all 'scan-csv "no quotes,'a ''quote''',\"another \"\"quote\"\"\"")
