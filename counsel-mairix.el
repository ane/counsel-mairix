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

;; counsel-mairix is an counsel interface for mairix.  Invoke `counsel-mairix' to start
;; a search with an counsel interface.

;;; Code:
(require 'cl-lib)
(require 'cl-generic)
(require 'mairix)
(require 'counsel)


;; Custom stuff.
(defgroup counsel-mairix nil
  "Options for counsel-mairix."
  :group :mail)

(defcustom counsel-mairix-mail-frontend nil
  "Mail program to display search results.
The default is to defer to `mairix-mail-program', which is probably a good idea,
because the format used by Mairix might not be compatible with
the frontend set here."
  :type '(choice (const :tag "Default (i.e. nil) to `mairix-mail-program'" nil)
                 (const :tag "RMail" rmail)
		 (const :tag "Gnus mbox" gnus)
		 (const :tag "VM" vm))
  :group 'counsel-mairix)


;; Generic methods that form the backbone of the search mechanism.
(cl-defgeneric counsel-mairix-run-search (frontend search-string)
  "Run Mairix with the search string SEARCH-STRING using FRONTEND.")

(cl-defgeneric counsel-mairix-display-result-message (message)
  "Display MESSAGE using the right frontend.")

(defun counsel-mairix-determine-frontend ()
  "Try to compute the frontend that the user of Mairix is using."
  (or counsel-mairix-mail-frontend
      mairix-mail-program))

(defun counsel-mairix-search-file ()
  "Get the full path to the Mairix search file as given by `mairix-file-path' and `mairix-search-file."
  (concat (file-name-as-directory (expand-file-name mairix-file-path))
          mairix-search-file))


;;; Rmail implementation of counsel-mairix.

(cl-defstruct counsel-mairix-rmail-result
  "A Mairix result entry to be displayed in Rmail."
  mbox-file msgnum)

(cl-defmethod counsel-mairix-run-search ((frontend (eql rmail)) search-string)
  "Perform a Mairix search using SEARCH-STRING using Rmail as the displaying FRONTEND."
  (let ((config (current-window-configuration))
        (search-file (counsel-mairix-search-file))
        sumbuf rmailbuf)
    (progn
      (save-excursion
        (mairix-call-mairix search-string nil nil)
        ;; The search mbox might be open somewhere. Close it,
        ;; because its contents will change.
        (let ((searchbuffer (find-buffer-visiting search-file)))
          (when searchbuffer
            (kill-buffer searchbuffer)))
        (rmail-input search-file)
        (rmail-summary)
        (with-current-buffer rmail-buffer
          (setq rmailbuf rmail-buffer)
          (setq sumbuf rmail-summary-buffer)))
      (when (and rmailbuf sumbuf)
        (let (results)
          (with-current-buffer sumbuf
            (font-lock-ensure)
            (setq results (split-string (buffer-string) "\n")))
          (kill-buffer rmailbuf)
          ;; The summary buffer might still be open. Kill it.
          (when sumbuf
            (kill-buffer sumbuf))
          (set-window-configuration config)
          (mapcar
           (lambda (str)
             (when-let ((num (string-to-number (substring str 0 6))))
               ;; Counsel doesn't support rich results so we have to stuff things into
               ;; text properties.
               (propertize str 'result (make-counsel-mairix-rmail-result :msgnum num :mbox-file search-file))
               ))
           (seq-remove #'string-empty-p results)))))))

(cl-defmethod counsel-mairix-display-result-message ((result counsel-mairix-rmail-result))
  (rmail (counsel-mairix-rmail-result-mbox-file result))
  (rmail-show-message (counsel-mairix-rmail-result-msgnum result)))


;; Gnus implementation of the generic methods.
;; TODO...


;; The main implementation.

(defun counsel-mairix-do-search (str)
  "Either wait for more chars using `ivy-more-chars' or perform the search using STR after determining the correct search backend."
  (or (ivy-more-chars)
      (counsel-mairix-run-search (counsel-mairix-determine-frontend) str)
      '("" "Searching...")))

(cl-defmethod counsel-mairix-display-result-message ((result string))
  "Dispatch to `counsel-mairix-display-result-message' using the RESULT class stored in the 'result property of the search result, since the result class is stored there."
  (when-let (res (get-text-property 0 'result result))
    (counsel-mairix-display-result-message res)))


;;;###autoload
(defun counsel-mairix (&optional initial-input)
  "Search using Mairix with an Counsel frontend.
It will determine the correct backend automatically based on the variable
`mairix-mail-program', this can be overridden using
`counsel-mairix-mail-frontend'.

counsel-mairix should support the same backends as mairix itself,
which are known to be Rmail (default), Gnus and VM. Currently
only Rmail is supported."
  (interactive)
  (ivy-read "Mairix query: " #'counsel-mairix-do-search
            :initial-input initial-input
            :dynamic-collection t
            :action #'counsel-mairix-display-result-message
            :history 'counsel-mairix-history
            :caller 'counsel-mairix))

(provide 'counsel-mairix)

;;; counsel-mairix.el ends here
