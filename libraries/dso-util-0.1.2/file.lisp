#|
Copyright (C) 2008  David Owen <dsowen@fugue88.ws>

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

(in-package #:dso-util)



(defun gulp-input (&optional (stream *standard-input*))
  (let ((s-out (make-string-output-stream))
        (buffer (make-sequence 'string 4096)))
    (do ((n #1=(read-sequence buffer stream) #1#))
        ((= n 0))
      (write-sequence buffer s-out :end n))
    (get-output-stream-string s-out)))
