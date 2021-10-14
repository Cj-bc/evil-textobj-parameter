(evil-define-text-object evil-textobj-parameter-inner-parameter (count &optional beg end type)
  "Text object for function parameter"
  ; :type inclusive
  (let ((beg (progn (re-search-backward (rx (any ",(")))
		    (forward-char 2)
		    (point)))
	(end (progn (re-search-forward (rx (any ",)")))
		    (backward-char 1)
		    (point))))
    (list beg end)
    )
  )

(evil-define-text-object evil-textobj-parameter-outer-parameter (count &optional beg end type)
  "Text object for function parameter. But it includes comma"
  (cond ((evil-textobj-parameter--is-first-parameter) (evil-textobj-parameter--first-parameter-pos))
	; check 'first-parameter' first or 'can't find ","' error will occur
	((evil-textobj-parameter--is-last-parameter) (evil-textobj-parameter--last-parameter-pos))
	(t (save-excursion
	      (search-backward ",")
	      (backward-char 1)
	      (re-search-forward (rx "," (*? (not ",")) ","))
	      (list (match-beginning 0) (- (match-end 0) 1))))
	))

(defun evil-textobj-parameter--inside (pos beg end)
  "shorthand for beg <= pos <= end"
  (and (<= beg pos)
       (<= pos end)))

(defun evil-textobj-parameter--last-parameter-pos ()
  "Return list of beggining of next last parameter and ending of it.
last parameter means last parameter of any declarations
This will update match data"
  (save-excursion
    (let ((re-last-param (rx ","
			     (*? (not ","))
			     (syntax close-parenthesis)))
	   )
      (search-backward ",")
      (re-search-forward re-last-param)
      (list (match-beginning 0) (- (match-end 0) 1)))))
		       
					; I need to `(match-end 0) - 2' because
					; re-last-param include close paren
 


(defun evil-textobj-parameter--is-last-parameter ()
  "Return 't if cursor is now on last parameter
This will update match data"
  (eval `(evil-textobj-parameter--inside (point)
				   ,@(evil-textobj-parameter--last-parameter-pos))))


(defun evil-textobj-parameter--first-parameter-pos ()
  "Return pair of beggining and ending of first parameter it found"
  (save-excursion
    (let ((re-first-param (rx (syntax open-parenthesis)
			      (*? (not ","))
			      ","
			      (* (syntax whitespace))))
	  )
      (search-forward ",")
      (forward-char 2)
      (re-search-backward re-first-param)
      (list (+ 1 (match-beginning 0)) ; I need `+ 1' because it includes '(' character
	    (match-end 0)))))
					; I intentionally don't 

(defun evil-textobj-parameter--is-first-parameter ()
  "Return 't if cursor is now on last parameter
This will update match data"
  (eval `(evil-textobj-parameter--inside (point)
					  ,@(evil-textobj-parameter--first-parameter-pos))))
