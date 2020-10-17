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

;; tests for counsel-mairix.

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'seq)
(require 'mairix)

;;; Some basic tests to ensure mairix is working correctly.
(ert-deftest test-mairix-presence ()
  "Test whether mairix is installed on the system."
  (should (not (null (executable-find "mairix")))))

(ert-deftest test-packages ()
  "Test whether counsel and mairix are installed."
  (should (and (featurep 'counsel)
               (featurep 'mairix))))

(ert-deftest test-implementation-detection ()
  "Test whether implementation detection works correctly."
  (dolist (impl '(rmail gnus vm foo))
    (let ((mairix-mail-program impl))
      (should (eql impl (counsel-mairix-determine-frontend))))))

(defmacro with-test-mairix (&rest body)
  `(unwind-protect
       (progn
         (let ((mairix-command (format "mairix -f tests/mairixrc"))
               (mairix-search-file "test.mbox"))
           ,@body))
     (when-let (k (get-buffer "test.mbox"))
       (kill-buffer k))
     (dolist (file (list "tests/mairixdb" "test.mbox"))
       (when (file-exists-p file)
         (message "Deleting file %s." file)
         (delete-file file)))))

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

;;; counsel-mairix-tests.el ends here.

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
