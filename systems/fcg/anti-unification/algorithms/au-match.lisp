(in-package :fcg)

(defun au-match-structures (pattern source bsl)
  "anti-unify structures in matching"
  (setf pattern (remove-J-units pattern))
  (when (<= (length pattern) (length source))
    (cond ((null pattern) bsl)
	  ((<= (length pattern) (length source))
	   (subset-p pattern source bsl :unify-fn #'(lambda (u1 u2 bsl)
						      (unify-units u1 u2 bsl))))
	  (t +fail+))))