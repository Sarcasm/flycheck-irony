;;; flycheck-irony.el --- Flycheck: C/C++ support via Irony  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: convenience, tools, c
;; Version: 0.1.0-cvs
;; URL: https://github.com/Sarcasm/flycheck-irony/
;; Package-Requires: ((emacs "24.1") (flycheck "0.22-cvs1") (irony "0.2.0-cvs2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; C, C++ and Objective-C support for Flycheck, using Irony Mode.
;;
;; Usage:
;;
;;     (eval-after-load 'flycheck
;;       '(add-to-list 'flycheck-checkers 'irony))

;;; Code:

(require 'irony-diagnostics)

(require 'flycheck)

(defun flycheck-irony--build-error (checker buffer diagnostic)
  (let ((severity (irony-diagnostics-severity diagnostic)))
    (if (member severity '(note warning error fatal))
        (flycheck-error-new-at
         (irony-diagnostics-line diagnostic)
         (irony-diagnostics-column diagnostic)
         (cdr (assoc severity '((note    . info)
                                (warning . warning)
                                (error   . error)
                                (fatal   . error))))
         (irony-diagnostics-message diagnostic)
         :checker checker
         :buffer buffer
         :filename (irony-diagnostics-file diagnostic)))))

(defun flycheck-irony--start (checker callback)
  (let ((buffer (current-buffer)))
    (irony-diagnostics--async
     #'(lambda () ;; closure, lexically bound
         (let ((errors (mapcar
                        #'(lambda (diagnostic)
                            (flycheck-irony--build-error checker buffer
                                                         diagnostic))
                        (irony-diagnostics))))
           (funcall callback 'finished (delq nil errors)))))))

(flycheck-define-generic-checker 'irony
  "A syntax checker for C, C++ and Objective-C, using Irony Mode."
  :start 'flycheck-irony--start
  :error-filter #'identity
  :predicate #'(lambda ()
                 irony-mode))

(provide 'flycheck-irony)

;;; flycheck-irony.el ends here
