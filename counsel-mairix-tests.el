;;; counsel-mairix.el --- Counsel interface for Mairix -*- lexical-binding: t -*-

;; Copyright (c) 2020 Antoine Kalmbach

;; Author: Antoine Kalmbach <ane@iki.fi>
;; Created: 2020-10-10
;; Version: 0.1
;; Keywords: mail searching
;; Package-Requires: ((emacs "26.1") (ivy "0.13.1"))

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

(defun ivy-input (expr keys)
  "Return `ivy-text' after inputing KEYS."
  (let ((ivy-expr expr)
        (buf (current-buffer)))
    (save-window-excursion
      (unwind-protect
          (execute-kbd-macro
           (vconcat (kbd "C-c e")
                    (kbd keys)))
        (switch-to-buffer buf)))
    ivy-text))


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
  `(let ((saved mairix-saved-searches))
     (unwind-protect
         (progn
           (let ((counsel-mairix-include-threads t)
                 (mairix-command (format "mairix -f %s" (expand-file-name "tests/mairixrc")))
                 (mairix-search-file "test.mbox"))
             ,@body))
       (when-let (k (get-buffer "test.mbox"))
         (kill-buffer k))
       (when (file-exists-p "test.mbox")
         (delete-file file))
       (setq mairix-saved-searches saved))))


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
           (ivy-test-inhibit-message t))
      (should (equal 88  (seq-length (ivy-with '(call-interactively 'counsel-mairix) no))))
      (should (equal 109 (seq-length (ivy-with '(call-interactively 'counsel-mairix) yes)))))))



(ert-deftest test-counsel-mairix-history ()
  "Test calling `counsel-mairix-save-search' should prompt for
the recently used searches."
  (with-test-mairix
    (let ((counsel-mairix-include-threads nil)
          (counsel-mairix-history nil))
      (ivy-with '(call-interactively 'counsel-mairix) "elpa C-m")
      (ivy-with '(call-interactively 'counsel-mairix) "f:rms C-m")

      (should (equal (ivy-with '(counsel-mairix-save-search) "C-m foo C-m y")
                     "elpa"))
      (should (equal (ivy-with '(counsel-mairix-save-search) "C-n C-m bar C-m y")
                     "f:rms")))))

(ert-deftest test-counsel-mairix-yanking ()
  (with-test-mairix
    (save-window-excursion
      (rmail "./tests/feb-2020-emacs-devel.mbox")
      (let ((from (mail-strip-quoted-names (rmail-get-header "from")))
            (to   (mail-strip-quoted-names (rmail-get-header "to")))
            (mid  (mail-strip-quoted-names (rmail-get-header "message-id"))))
        (should (string= from "rms@gnu.org"))
        (should (string= to "lg.zevlg@gmail.com"))
        (should (string= mid "E1j8FZ0-0004sb-I6@fencepost.gnu.org"))

        (should (equal from (counsel-mairix--get-field "from")))
        (should (equal to (counsel-mairix--get-field "to")))
        (should (equal mid (counsel-mairix--get-field "message-id")))

        (should (equal (ivy-input '(counsel-mairix) "C-c C-f f")
                       "f:rms@gnu.org"))
        (should (equal (ivy-input '(counsel-mairix) "C-c C-f t")
                       "t:lg.zevlg@gmail.com"))
        (should (equal (ivy-input '(counsel-mairix) "C-c C-f i")
                       "m:E1j8FZ0-0004sb-I6@fencepost.gnu.org"))))))

(ert-deftest test-counsel-mairix-insert-saved-searches ()
  (with-test-mairix
    (let ((mairix-saved-searches '(("a" . "s:elpa") ("b" . "f:rms@gnu.org"))))
      (should (equal (ivy-input '(call-interactively 'counsel-mairix)
                                "s:elpa")
                     "s:elpa"))
      (should (equal (ivy-input '(call-interactively 'counsel-mairix)
                                "f:rms@gnu.org")
                     "f:rms@gnu.org")))))

(ert-deftest test-counsel-mairix-save-current-search ()
  (with-test-mairix
    (let ((mairix-saved-searches nil))
      (ivy-with '(counsel-mairix) "s:elpa C-c C-s s foo C-m y")
      (should (equal '(("foo" . "s:elpa")) mairix-saved-searches)))))

(ert-deftest test-counsel-mairix-search-from ()
  (with-test-mairix
    (rmail "./tests/feb-2020-emacs-devel.mbox")
    (should (equal (ivy-input '(call-interactively 'counsel-mairix-search-from) "")
                   "f:rms@gnu.org"))))

(ert-deftest test-counsel-mairix-search-thread ()
  (with-test-mairix
    (rmail "./tests/feb-2020-emacs-devel.mbox")
    (should (equal (ivy-input '(call-interactively 'counsel-mairix-search-thread) "")
                   "m:E1j8FZ0-0004sb-I6@fencepost.gnu.org"))))

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

(provide 'counsel-mairix-tests)
;;; counsel-mairix-tests.el ends here.
