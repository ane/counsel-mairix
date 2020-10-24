;;; counsel-mairix.el --- Counsel interface for Mairix -*- lexical-binding: t -*-

;; Copyright (c) 2020 Antoine Kalmbach

;; Author: Antoine Kalmbach <ane@iki.fi>
;; Created: 2020-10-10
;; Version: 0.1
;; Keywords: mail searching
;; Package-Requires: ((emacs "26.1") (counsel "0.13.1"))

;; This file is not part of GNU Emacs.

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

;; Tests for counsel-mairix.  The ivy- bits are copyright (c) 2015-2019 Oleh
;; Krehel.

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'seq)
(require 'mairix)
(require 'counsel-mairix)


;;; From https://github.com/abo-abo/swiper/blob/master/ivy-test.el
(defvar ivy-expr nil
  "Holds a test expression to evaluate with `ivy-eval'.")

(defvar ivy-result nil
  "Holds the eval result of `ivy-expr' by `ivy-eval'.")

(defvar ivy-eval-dir nil
  "Hold the `default-directory' value to be used by `ivy-eval'.
Since `execute-kbd-macro' doesn't pick up a let-bound `default-directory'.")

(defun ivy-eval ()
  "Evaluate `ivy-expr'."
  (interactive)
  (let ((default-directory (or ivy-eval-dir default-directory)))
    (setq ivy-result (eval ivy-expr))))

(global-set-key (kbd "C-c e") 'ivy-eval)

(defvar ivy-test-inhibit-message t)

(cl-defun ivy-with (expr keys &key dir)
  "Evaluate EXPR followed by KEYS."
  (let ((ivy-expr expr)
        (inhibit-message ivy-test-inhibit-message)
        (buf (current-buffer)))
    (save-window-excursion
      (unwind-protect
          (progn
            (when dir
              (setq dir (expand-file-name dir)))
            (setq ivy-eval-dir dir)
            (execute-kbd-macro
             (vconcat (kbd "C-c e")
                      (kbd keys))))
        (switch-to-buffer buf)))
    ivy-result))


;;; Some basic tests to ensure mairix is working correctly.
(ert-deftest test-mairix-presence ()
  "Test whether mairix is installed on the system."
  (should (not (null (executable-find "mairix")))))

(ert-deftest test-packages ()
  "Test whether counsel and mairix are installed."
  (should (and (featurep 'ivy)
               (featurep 'mairix))))

(ert-deftest test-implementation-detection ()
  "Test whether implementation detection works correctly."
  (dolist (impl '(rmail gnus vm foo))
    (let ((mairix-mail-program impl))
      (should (eql impl (counsel-mairix-determine-frontend))))))


;;; Test fixtures and the like.
(defmacro with-test-mairix (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (let ((mairix-command (format "mairix -f tests/mairixrc"))
               (mairix-search-file "test.mbox"))
           ,@body))
     (when-let (k (get-buffer "test.mbox"))
       (kill-buffer k))
     (when (file-exists-p "test.mbox")
       (delete-file file))))


(ert-deftest test-implementation-override ()
  "Test whether implementation override works correctly."
  (let ((mairix-mail-program 'foobar))
    (eql 'foobar (counsel-mairix-determine-frontend))))

(ert-deftest test-baseline-mairix-search ()
  "Test whether our 'standard query' works as expected."
  (ert-with-message-capture msgs
    (with-test-mairix
      ;; you'd think the word 'lisp' would'be mentioned more than just 187 times in
      ;; one month on emacs-devel, huh?
      (mairix-search "lisp" t))
    (should (cl-search "Matched 187 messages" msgs))))




(ert-deftest test-counsel-mairix-threads-customization ()
  "Test whether changing `counsel-mairix-include-threads' affects the behavior
of `counsel-mairix'."
  (with-test-mairix
    (let ((counsel-mairix-include-threads nil))
      (let ((result (ivy-with '(call-interactively 'counsel-mairix) "melpa")))
        (should (equal 74 (seq-length result)))
        (should (cl-search "pogonyshev" result))))
    (let ((counsel-mairix-include-threads t))
      (let ((result (ivy-with '(call-interactively 'counsel-mairix) "elpa")))
        (should (equal 81 (seq-length result)))
        (should (cl-search "monnier" result))))))

(ert-deftest test-counsel-mairix-prompt-answer ()
  "Test whether the answering y or n at the prompt actually does anything."
  (with-test-mairix
    (let* ((searchable (if noninteractive " C-m lisp" "lisp"))
           (yes (concat "y" searchable))
           (no  (concat "n" searchable))
           (counsel-mairix-include-threads 'prompt)
           (ivy-test-inhibit-message nil))
      (should (equal 88  (seq-length (ivy-with '(call-interactively 'counsel-mairix) no))))
      (should (equal 109 (seq-length (ivy-with '(call-interactively 'counsel-mairix) yes)))))))



(ert-deftest test-counsel-mairix-history ()
  (with-test-mairix
    (let ((counsel-mairix-include-threads nil))
      (ivy-with '(call-interactively 'counsel-mairix) "elpa C-m")
      (ivy-with '(call-interactively 'counsel-mairix) "f:rms C-m")

      (should (equal (ivy-with '(counsel-mairix-save-search) "C-m foo C-m y")
                     "elpa"))
      (should (equal (ivy-with '(counsel-mairix-save-search) "C-n C-m bar C-m y")
                     "f:rms")))))


;;; counsel-mairix-tests.el ends here.

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
