;;; leap.el --- Leap exercise (exercism)

;;; Commentary:

;;; Code:

(defun divisible-p (a b)
  "Is A evenly divisible by B?"
  (= (% a b) 0))

(defun leap-year-p (year)
  "Is the provided YEAR a Gregorian leap year?"
  (cond ((divisible-p year 400) t)
        ((divisible-p year 100) nil)
        ((divisible-p year 4) t)
        (t nil)))

(provide 'leap-year-p)
;;; leap.el ends here
