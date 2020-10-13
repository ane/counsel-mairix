;;; ivy-mairix.el --- Ivy interface for Mairix -*- lexical-binding: t -*-

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

;; ivy-mairix is an ivy interface for mairix.  Invoke `ivy-mairix' to start
;; a search with an ivy interface.

;;; Code:
(require 'cl-lib)
(require 'cl-generic)


;; Custom stuff.
(defgroup ivy-mairix nil
  "Options for ivy-mairix."
  :group :mail)

(defcustom ivy-mairix-mail-frontend nil
  "Mail program to display search results.
The default is to defer to `mairix-mail-program', which is probably a good idea,
because the format used by Mairix might not be compatible with
the frontend set here."
  :type '(choice (const :tag "Default (i.e. nil) to `mairix-mail-program'" nil)
                 (const :tag "RMail" rmail)
		 (const :tag "Gnus mbox" gnus)
		 (const :tag "VM" vm))
  :group 'ivy-mairix)


;; Generic methods that form the backbone of the search mechanism.
(cl-defgeneric ivy-mairix-run-search (frontend search-string)
  "Run Mairix with the search string SEARCH-STRING using FRONTEND.")

(cl-defgeneric ivy-mairix-display-result-message (message)
  "Display MESSAGE using the right frontend.")

(defun ivy-mairix-determine-frontend ()
  "Try to compute the frontend that the user of Mairix is using."
  (or ivy-mairix-mail-frontend
      mairix-mail-program))

(defun ivy-mairix-search-file ()
  "Get the full path to the Mairix search file as given by `mairix-file-path' and `mairix-search-file."
  (concat (file-name-as-directory (expand-file-name mairix-file-path))
          mairix-search-file))


;;; Rmail implementation of ivy-mairix.

(cl-defstruct ivy-mairix-rmail-result
  "A Mairix result entry to be displayed in Rmail."
  mbox-file msgnum)

(cl-defmethod ivy-mairix-run-search ((frontend (eql rmail)) search-string)
  "Perform a Mairix search using SEARCH-STRING using Rmail as the displaying FRONTEND."
  (let ((config (current-window-configuration))
        (search-file (ivy-mairix-search-file))
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
               ;; Ivy doesn't support rich results so we have to stuff things into
               ;; text properties.
               (propertize str 'result (make-ivy-mairix-rmail-result :msgnum num :mbox-file search-file))
               ))
           (seq-remove #'string-empty-p results)))))))

(cl-defmethod ivy-mairix-display-result-message ((result ivy-mairix-rmail-result))
  (rmail (ivy-mairix-rmail-result-mbox-file result))
  (rmail-show-message (ivy-mairix-rmail-result-msgnum result)))


;; Gnus implementation of the generic methods.
;; TODO...


;; The main implementation.

(defun ivy-mairix-do-search (str)
  "Either wait for more chars using `ivy-more-chars' or perform the search using STR after determining the correct search backend."
  (or (ivy-more-chars)
      (ivy-mairix-run-search (ivy-mairix-determine-frontend) str)
      '("" "working...")))

(cl-defmethod ivy-mairix-display-result-message ((result string))
  "Dispatch to `ivy-mairix-display-result-message' using the RESULT class stored in the 'result property of the search result, since the result class is stored there."
  (when-let (res (get-text-property 0 'result result))
    (ivy-mairix-display-result-message res)))


;;;###autoload
(defun ivy-mairix (&optional initial-input)
  "Search using Mairix with an Ivy frontend.
It will determine the correct backend automatically based on the variable
`mairix-mail-program', this can be overridden using
`ivy-mairix-mail-frontend'.

ivy-mairix should support the same backends as mairix itself,
which are known to be Rmail (default), Gnus and VM. Currently
only Rmail is supported."
  (interactive)
  (ivy-read "Search: " #'ivy-mairix-do-search
            :initial-input initial-input
            :dynamic-collection t
            :action #'ivy-mairix-display-result-message
            :history 'ivy-mairix-history
            :caller 'ivy-mairix))

(provide 'ivy-mairix)

;;; ivy-mairix.el ends here
