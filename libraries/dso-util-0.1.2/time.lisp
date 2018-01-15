#|
Copyright (C) 2007  David Owen <dsowen@fugue88.ws>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
|#

(in-package #:dso-util)



(defun parse-iso-date (s)
  (let ((broken
	 (nth-value 1 (scan-to-strings "^(\\d{4})-(\\d{2})-(\\d{2})$" s))))
    (let ((parts (map 'list #'parse-integer broken)))
      (destructuring-bind (year month day) parts
	(encode-universal-time 0 0 0 day month year)))))

(defun format-iso-date (date)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time date)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
