;;; magit-blame-color-by-age.el --- Color magit-blame headings by age -*- lexical-binding: t -*-
;; Copyright (C) 2025  J.D. Smith
;; Author: J.D. Smith
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
(require 'cl-lib)
(require 'color)
(require 'magit-blame)
(require 'magit-section)

(defcustom mbc/full-heading nil
  "Whether to color the background of the entire header.
If nil, only the data within the heading is affected."
  :type 'boolean
  :group 'magit-blame)

(defcustom mbc/colors (cons "blue" "red")
  "Color name for ages, from oldest to newest.
These colors are blended with the background color; see
`magit-blame-color-by-age-frac'"
  :type '(cons string string)
  :group 'magit-blame)

(defcustom mbc/frac 0.35
  "Fractional contribution to heading background color of age-based color.
Must be between 0 and 1."
  :type 'float
  :group 'magit-blame)

(defun mbc/-blend (from to frac back)
  "Select a color at FRAC from FROM to TO, blending with background BACK.
All colors are (R G B) triples."
  (cl-labels
      ((combine (rgb1 rgb2 frac)
	 (cl-mapcar (lambda (c1 c2) (+ (* (- 1. frac) c1) (* frac c2))) rgb1 rgb2)))
    (apply #'color-rgb-to-hex (combine back (combine from to frac) mbc/frac))))

(defun mbc/update (&optional beg end)
  "Update `magit-blame' headings between BEG and END with age-based colors.
Defaults to the full buffer."
  (save-restriction
    (widen)
    (let* ((seen (make-hash-table))
	   (hformat (magit-blame--style-get 'heading-format))
	   (string-key (list hformat '(magit-blame-heading default)))
	   (age-key (if (string-search "%C" hformat) "committer-time" "author-time"))
	   (back-col (color-name-to-rgb (face-background 'magit-blame-heading nil t)))
	   (from-col (color-name-to-rgb (car mbc/colors)))
	   (to-col (color-name-to-rgb (cdr mbc/colors)))
	   age-min age-rng)
      (cl-loop for v being the hash-values of magit-blame-cache
	       for tmsstr = (cdr (assoc age-key v))
	       for tm = (and tmsstr (string-to-number tmsstr))
	       if tm maximize tm into mx and minimize tm into mn
	       finally (setq age-min mn age-rng (max 1 (- mx mn))))
      (dolist (ov (overlays-in (or beg (point-min))
                               (or end (point-max))))
	(when-let* (( (overlay-get ov 'magit-blame-heading))
		    (revinfo (overlay-get ov 'magit-blame-revinfo))
		    (string (cdr (assoc string-key revinfo)))
		    ( (not (gethash string seen)))
		    (age-str (cdr (assoc age-key revinfo))))
	  (puthash string t seen)
	  (let* ((age-frac (/ (float (- (string-to-number age-str) age-min)) age-rng))
		 (age-color (mbc/-blend from-col to-col age-frac back-col)))
	    (if mbc/full-heading
		(magit--add-face-text-property
		 0 (length string) `(:background ,age-color :extend t) nil string)
	      (cl-loop
	       for i being the intervals of string property 'font-lock-face
	       for props = (get-text-property (car i) 'font-lock-face string)
	       if (memq 'magit-blame-date props) do
	       (if (eq (cadr props) :background)
		   (setcdr (car props) age-color)
		 (put-text-property (car i) (cdr i) 'font-lock-face
				    (cons `(:background ,age-color) props) string))))))))))

(defun mbc/-sentinel (process &rest _r)
  "A sentinel for PROCESS to update `magit-blame' heading colors by age."
  (when (and (eq (process-status process) 'exit)
	     (zerop (process-exit-status process)))
    (with-current-buffer (process-get process 'command-buf)
      (mbc/update)
      (font-lock-flush))))

(define-minor-mode mbc/mode
  "Color `magit-blame' headers by age."
  :global t
  :group 'magit-blame
  (if mbc/mode
      (advice-add #'magit-blame-process-sentinel :after #'mbc/-sentinel)
    (advice-remove #'magit-blame-process-sentinel #'mbc/-sentinel)))

(provide 'magit-blame-color-by-age)
;;; magit-blame-color-by-age.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("mbc/" . "magit-blame-color-by-age-"))
;; End:
