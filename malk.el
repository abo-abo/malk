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

;;

;;; Code:
(require 'orly)

(defvar-local malk-rules nil)
(defvar-local malk-action nil)

(defun malk-exit-function (string status)
  (just-one-space))

(defun malk-complete-file-name (&rest _)
  (orly-completion-filesystem))

(defun malk-completion-at-point ()
  (catch 'result
    (dolist (rule malk-rules)
      (when (looking-back (car rule) (line-beginning-position))
        (throw 'result
          (if (functionp (cadr rule))
              (funcall (cadr rule))
            (list (match-beginning 0)
                  (match-end 0)
                  (all-completions (match-string 0) (cadr rule))
                  :exit-function #'malk-exit-function)))))))

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
 '(("#\\(\\sw*\\)" ("#emacs" "#is" "#cool"))
   ("file:\\(\\(?:\\sw\\|\\s_\\|~\\)*\\)" malk-complete-file-name))
 #'message
 :initial-message
 "You can insert any amount of \"#emacs\", \"#is\", \"#cool\", or \"file:~/Documents\".
Enter \"#\" or \"file:PATH\" and press TAB for completion. Press RET to submit\n")

(provide 'malk)
;;; malk.el ends here
