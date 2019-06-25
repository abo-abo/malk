;;; malk.el --- Select many candidates from many completion sources  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Now with vitamin R.
;; This package is a simple demo of `completion-at-point-functions'.

;;; Code:
(require 'orly)

(defvar-local malk-rules nil)
(defvar-local malk-action nil)

(defun malk-just-one-space (string status)
  (just-one-space))

(defun malk-complete-file-name (&rest _)
  (orly-completion-filesystem))

(defun malk-completion-at-point ()
  (catch 'result
    (dolist (rule malk-rules)
      (let ((rule-regex (car rule))
            (rule-body (cadr rule)))
        (when (looking-back rule-regex (line-beginning-position))
          (throw 'result
            (let ((collection (car rule-body)))
              (if (functionp collection)
                  (funcall collection)
                (append
                 (list
                  (match-beginning 0)
                  (match-end 0)
                  (all-completions (match-string 0) collection))
                 (cdr rule-body))))))))))

(defun malk-done ()
  (interactive)
  (funcall malk-action
           (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position))))

(define-derived-mode malk-mode fundamental-mode "multi-complete"
  "A special mode for completing multiple fields."
  (setq-local completion-at-point-functions '(malk-completion-at-point)))

(define-key malk-mode-map (kbd "TAB") 'complete-symbol)
(define-key malk-mode-map (kbd "C-m") 'malk-done)

(cl-defun malk-completing-read (rules action &keys initial-message)
  (let ((buf (get-buffer-create "*multi-complete*")))
    (pop-to-buffer buf)
    (erase-buffer)
    (malk-mode)
    (when initial-message
      (insert initial-message))
    (setq malk-rules rules)
    (setq malk-action action)))

(malk-completing-read
 '(("#\\(\\sw*\\)" (("#emacs" "#is" "#cool") :exit-function malk-just-one-space))
   ("file:\\(\\(?:\\sw\\|\\s_\\|~\\)*\\)" (malk-complete-file-name))
   ("" (("#" "file:"))))
 (lambda (str)
   (save-excursion
     (animate-string str 10)))

 :initial-message
 "You can insert any amount of \"#emacs\", \"#is\", \"#cool\", or \"file:~/Documents\".
Keep pressing TAB for completion. Press RET to submit\n")

(provide 'malk)
;;; malk.el ends here
