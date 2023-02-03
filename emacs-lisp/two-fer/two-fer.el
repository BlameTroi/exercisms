;;; two-fer.el --- Two-fer Exercise (exercism)

;;; Commentary:

;;; Code:

(defun two-fer (&optional name)
  "Two-fer exercism assignment.
Given an optional NAME, return a sentence of the 'one for you two for me' form."
  (interactive "sName? ")
  (cond ((equal name nil) (message "One for you, one for me."))
        (t (message "One for %s, one for me." name))))

(provide 'two-fer)
;;; two-fer.el ends here
