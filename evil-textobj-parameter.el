;;; evil-textobj-parameter.el --- Port of vim-textobj-parameter

;; Copyright (c) 2021,2023 Cj.bc-sd a.k.a. Cj-bc

;; Author: Cj.bc-sd a.k.a. Cj-bc
;; Maintainer: Cj.bc-sd a.k.a. Cj-bc
;; Created: 14 Oct 2021
;; Keywords: evil
;; URL: https://github.com/Cj-bc/evil-textobj-parameter
;; Package-Requires: (evil)

;; This file is not part of GNU Emacs.

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Commentary:
;; This package is port of https://github.com/sgur/vim-textobj-parameter
;; 
;; This provides two text objects, one for inner textobject and another for outer.
;;
;; Quick usage:
;;   + Assign `evil-textobj-parameter-inner-parameter' and
;;     `evil-textobj-parameter-outer-parameter' to some key with `define-key'
;;
;; Example:
;;  (define-key evil-inner-text-objects-map "," 'evil-textobj-parameter-inner-parameter)
;;  (define-key evil-outer-text-objects-map "," 'evil-textobj-parameter-outer-parameter)

;; See README.org on github for more information
;;

;;; Code:
(require 'evil-macros)
(require 'evil-common)
(evil-define-text-object evil-textobj-parameter-inner-parameter (count &optional beg end type)
  "Text object for function parameter"
  ; :type inclusive
  ;; 一番内側の括弧よりも外側のカンマを探索してしまわないようにする。
  (let* ((nearest-paren (evil-select-paren (rx (syntax open-parenthesis))
					   (rx (syntax close-parenthesis))
					   beg end type nil t))
	 (beg (save-excursion
		(let ((keepLoop t))
		  (when (thing-at-point 'evil-quote)
		    (goto-char (car (bounds-of-thing-at-point 'evil-quote))))
		  (while (and keepLoop (re-search-backward
					(rx (or (any ",\"") (syntax close-parenthesis) (syntax open-parenthesis)))
					(nth 0 nearest-paren) t))
		    (cl-case (char-syntax (char-after))
		      (?\) (skip-syntax-backward "^(")
		       (backward-char))
		      (?\" (skip-syntax-backward "^\"")
		       (backward-char))
		      (otherwise (setq keepLoop nil))
		      )))
		(1+ (point))))
  	 (end (save-excursion
		(let ((keepLoop t))
		  (when (thing-at-point 'evil-quote)
		    (goto-char (cdr (bounds-of-thing-at-point 'evil-quote))))
		  (while (and keepLoop (re-search-forward
					(rx (or (any ",\"") (syntax close-parenthesis) (syntax open-parenthesis)))
					(nth 1 nearest-paren) t))
		    (cl-case (char-syntax (char-before))
		      (?\( (skip-syntax-forward "^)") (forward-char))
		      (?\" (skip-syntax-forward "^\"") (forward-char))
		      (t (setq keepLoop nil)))))
  		(1- (point)))))
    (if (and beg end)
	(list beg end)
      (error "No parameter found"))
    )
  )

(evil-define-text-object evil-textobj-parameter-outer-parameter (count &optional beg end type)
  "Text object for function parameter. But it includes comma"
  (cond ((evil-textobj-parameter--is-first-parameter) (evil-textobj-parameter--first-parameter-pos 'selection))
	; check 'first-parameter' first or 'can't find ","' error will occur
	((evil-textobj-parameter--is-last-parameter) (evil-textobj-parameter--last-parameter-pos))
	(t (save-excursion
	     (forward-char 1)
	     (let ((nearest-paren (evil-select-paren (rx (syntax open-parenthesis))
						     (rx (syntax close-parenthesis))
						     beg end type nil t))
		   )
	       (cond ((and (search-backward "," (nth 0 nearest-paren) t)
			   (re-search-forward (rx "," (*? (not ",")) ",") (nth 1 nearest-paren) t)
			   )
		      (list (match-beginning 0) (- (match-end 0) 1)))
		     (t (error "No parameter found"))
		     ))))))


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
	  (nearest-paren (evil-select-paren (rx (syntax open-parenthesis))
					   (rx (syntax close-parenthesis))
					   beg end type nil t))
	   )
      (when (and (search-backward "," (nth 0 nearest-paren) t)
		 (re-search-forward re-last-param (nth 1 nearest-paren) t))
	(list (match-beginning 0) (- (match-end 0) 1))))))
		       
					; I need to `(match-end 0) - 2' because
					; re-last-param include close paren
 

;;
(defun evil-textobj-parameter--is-last-parameter ()
  "Return 't if cursor is now on last parameter
This will update match data"
  (when (evil-textobj-parameter--last-parameter-pos)
    (eval `(evil-textobj-parameter--inside (point)
					   ,@(evil-textobj-parameter--last-parameter-pos)))))


;; 「このtextobjが呼ばれた時、今選択されている対象がfirst parameterであると判定する範囲」と
;; 「実際にfirst parameterとして取得する範囲」が異なるので、異なる実装をしている
(defun evil-textobj-parameter--first-parameter-pos (purpose)
  "Return pair of beggining and ending of first parameter it found
If the first parameter `purpose' is `search', this will return range that
should be used to test whether cursor selects thefirst parameter.
If it is `selection', this will return range that should be used to actually 'select' region"
  (save-excursion
    (let ((re-first-param (rx (syntax open-parenthesis)
			      (group (group (*? (not ","))) 
				     ","
				     (* (syntax whitespace)))))
	  (nearest-paren (evil-select-paren (rx (syntax open-parenthesis))
					    (rx (syntax close-parenthesis))
					    beg end type nil t))
          ; `re-first-param' に定義されているグループのうち、
          ; 一つ目のグループは実際に選択範囲される範囲で、
          ; 二つ目のグループは第一引数を選択していると判断する範囲
	  )
      (when (and (re-search-backward (rx (syntax open-parenthesis)) (nth 0 nearest-paren) t)
		 (re-search-forward re-first-param (nth 1 nearest-paren) t))
	(list (match-beginning 1)
	      (cond ((eq purpose 'selection) (match-end 1))
		    ((eq purpose 'search) (- (match-end 2) 1))
		    (t (error "Wrong symbol. possible symbols are: search, selection")))
	      )))))

(defun evil-textobj-parameter--is-first-parameter ()
  "Return 't if cursor is now on last parameter
This will update match data"
  (when (evil-textobj-parameter--first-parameter-pos 'search)
    (eval `(evil-textobj-parameter--inside (point)
					   ,@(evil-textobj-parameter--first-parameter-pos 'search)))))

(provide 'evil-textobj-parameter)
;;; evil-textobj-parameter.el ends here
