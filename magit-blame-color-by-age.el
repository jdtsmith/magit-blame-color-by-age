;;; magit-blame-color-by-age.el --- Color magit-blame headings by age -*- lexical-binding: t -*-
;; Copyright (C) 2025  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/magit-blame-color-by-age
;; Package-Requires: ((emacs "29.1") (magit))
;; Version: 0.2.0
;; Keywords: convenience

;; magit-blame-color-by-age is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; magit-blame-color-by-age is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Color magit-blame chunk headings by committer or author age.  Age
;; coloring is performed after the asynchronous magit-blame process
;; completes.  A "quickstart" run of visibile chunks occurs prior to
;; full completion; headings will receive further updates after this
;; initial run.  See the custom variables for a few configuration
;; options.

;;; Code:
(require 'font-lock)
(require 'cl-lib)
(require 'color)
(require 'magit-blame)
(require 'magit-section)

(defun mbc/-custom-set (sym val)
  "Set SYM to VAL and redefine blame faces."
  (set-default-toplevel-value sym val)
  (when (fboundp 'mbc/define-faces)
    (mbc/define-faces t)))

(defcustom mbc/full-heading nil
  "Whether to color the background of the entire header.
If nil, only the date information within the heading is affected."
  :type 'boolean
  :group 'magit-blame)

(defcustom mbc/fringe t
  "Whether to color the fringe of the block.
This is especially useful for styles with no header."
  :type 'boolean
  :group 'magit-blame)

(defcustom mbc/colors (cons "blue" "red")
  "Color name for ages, from oldest to newest.
These colors are blended with the background color; see
`magit-blame-color-by-age-frac'"
  :type '(cons string string)
  :group 'magit-blame
  :set #'mbc/-custom-set)

(defcustom mbc/frac 0.4
  "Fractional contribution to heading background color of age-based color.
Must be between 0 and 1."
  :type 'float
  :group 'magit-blame
  :set #'mbc/-custom-set)

(defcustom mbc/steps 32
  "Number of color steps to span from oldest to newest.
Maximum is 1000."
  :type 'natnum
  :group 'magit-blame
  :set #'mbc/-custom-set)

(defun mbc/-face (frac)
  "Return the appropriate face for age fraction FRAC (0-1)."
  (intern (format "magit-blame-color-by-age-%03d"
		  (round (* frac (1- mbc/steps))))))

(defun mbc/-blend (from to frac back)
  "Select a color at FRAC from FROM to TO, blending with background BACK.
All colors are (R G B) triples."
  (cl-labels
      ((combine (rgb1 rgb2 frac)
	 (cl-mapcar (lambda (c1 c2) (+ (* (- 1. frac) c1) (* frac c2))) rgb1 rgb2)))
    (apply #'color-rgb-to-hex (combine back (combine from to frac) mbc/frac))))

(defun mbc/define-faces (&optional redefine)
  "Define the blame color faces, if not defined or REDEFINE is non-nil.
Also defines the fringe bitmap."
  (when (and (or redefine (not (facep 'mbc/0)))
	     (boundp 'mbc/colors) (boundp 'mbc/steps))
    (define-fringe-bitmap 'mbc/fringe-bitmap (vconcat '(0)) nil nil '(top t))
    (let* ((from-col (color-name-to-rgb (car mbc/colors)))
	   (to-col (color-name-to-rgb (cdr mbc/colors)))
	   (back-col-name (or (face-background 'magit-blame-heading nil t)
			      (face-background 'default)))
	   (back-col (color-name-to-rgb back-col-name)))
      (dotimes (i mbc/steps)
	(let ((face (intern (format "magit-blame-color-by-age-%03d" i))))
	  (make-face face)
	  (set-face-extend face t)
	  (set-face-background face (mbc/-blend from-col to-col
						(/ (float i) (1- mbc/steps))
						back-col)))))))

;; This needs to run asynchronously, perhaps as a font-lock backend
;; (e.g. a font-lock-fontify-region-function).  So whenever we attempt
;; to fontify a region, we also check for and age-color the magit
;; overlays.  Then the sentinel can just flush everything.  For some
;; reason it runs twice, once quickly and once again.  I think magit
;; must emphasize the local region in its first run.  Look into.
;; Can take 10s of seconds to minutes in a big files like xdisp.c

(cl-defstruct (mbc/info (:constructor nil) (:constructor mbc/info))
  "Ages of the chunks in the buffer."
  age-min age-range age-key str-key (seen (make-hash-table)))

(defvar-local mbc/-info nil)

(defun mbc/update-info ()
  "Update `magit-blame' ages and keys in the buffer.
Also creates a new seen entity hash."
  (interactive)
  (let* ((hformat (let-alist magit-blame-styles .headings.heading-format))
	 (str-key (list hformat '(magit-blame-heading default)))
	 (age-key (if (string-search "%C" hformat) "committer-time"
		    "author-time")))
    (cl-loop for v being the hash-values of magit-blame-cache
	     for tmsstr = (cdr (assoc age-key v))
	     for tm = (and tmsstr (string-to-number tmsstr))
	     if tm maximize tm into mx and minimize tm into mn
	     finally (setq mbc/-info
			   (mbc/info :age-min mn
				     :age-range (max 1 (- mx mn))
				     :age-key age-key
				     :str-key str-key)))))

(defun mbc/-fontify-region (limit)
  "Apply chunk headline overlay fontification from point to LIMIT.
To be added as a font-lock keyword (although it works on `magit-blame's
overlays, not faces)."
  (pcase-let* (((cl-struct mbc/info
			   age-min age-range age-key str-key seen)
		mbc/-info))
    (dolist (ov (overlays-in (point) limit))
      ;; Full Heading or Date String in heading
      (when-let* (((overlay-get ov 'magit-blame-heading))
		  (revinfo (overlay-get ov 'magit-blame-revinfo))
		  (age-str (cdr (assoc age-key revinfo)))
		  (face (mbc/-face
			 (/ (float (- (string-to-number age-str) age-min)) age-range))))
	(when mbc/fringe
	  (overlay-put ov 'line-prefix
		       (propertize " " 'display
				   `((left-fringe mbc/fringe-bitmap ,face)))))
	(when-let* ((string (cdr (assoc str-key revinfo)))
		    ((not (gethash string seen))))
	  (puthash string t seen)
	  (if mbc/full-heading
	      (magit--add-face-text-property 0 (length string) face nil string)
	    (cl-loop
	     for i being the intervals of string property 'font-lock-face
	     for props = (get-text-property (car i) 'font-lock-face string)
	     for has-face = (string-prefix-p "magit-blame-color-by-age-"
					     (symbol-name (car props)))
	     if (memq 'magit-blame-date props) do
	     (if has-face
		 (setcar props face) 	; replace the face
	       (put-text-property (car i) (cdr i) 'font-lock-face
				  (cons face props) string))
	     else do
	     (when has-face
	       (put-text-property (car i) (cdr i) 'font-lock-face
				  (cdr props) string))))))))
  nil)

(defun mbc/-sentinel (process &rest _r)
  "A sentinel for PROCESS to update `magit-blame' heading colors by age."
  (when (and (eq (process-status process) 'exit)
	     (zerop (process-exit-status process)))
    (with-current-buffer (process-get process 'command-buf)
      (mbc/update-info)
      (font-lock-flush))))

(defun mbc/-setup-font-lock ()
  "Setup or teardown magit blame age coloring."
  (if magit-blame-mode
      (font-lock-add-keywords nil '(mbc/-fontify-region))
    (font-lock-remove-keywords nil '(mbc/-fontify-region))))

;;;###autoload
(define-minor-mode mbc/mode
  "Color `magit-blame' headers by age."
  :global t
  :group 'magit-blame
  (if mbc/mode
      (progn
	(add-hook 'magit-blame-mode-hook #'mbc/-setup-font-lock)
	(advice-add #'magit-blame-process-sentinel :after #'mbc/-sentinel)
	(mbc/define-faces))
    (advice-remove #'magit-blame-process-sentinel #'mbc/-sentinel)
    (remove-hook 'magit-blame-mode-hook #'mbc/-setup-font-lock)))

(provide 'magit-blame-color-by-age)
;;; magit-blame-color-by-age.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("mbc/" . "magit-blame-color-by-age-"))
;; package-lint--sane-prefixes: "^mbc/"
;; End:
