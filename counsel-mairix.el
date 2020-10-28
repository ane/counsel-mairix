;;; counsel-mairix.el --- Counsel interface for Mairix -*- lexical-binding: t -*-

;; Copyright (c) 2020 Antoine Kalmbach

;; Author: Antoine Kalmbach <ane@iki.fi>
;; Created: 2020-10-10
;; Version: 0.1
;; Keywords: mail
;; Package-Requires: ((emacs "26.3") (ivy "0.13.1"))

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

;; counsel-mairix is an ivy interface for mairix.  Invoke `counsel-mairix' to
;; start a search with an ivy interface.  counsel-mairix builds upon the
;; built-in mairix support in Emacs, adding a fast interactive searching
;; mechanism using the ivy completion engine.
;;
;; counsel-mairix provides the following functions:
;;
;;   * `counsel-mairix' - run mairix search interactively
;;   * `counsel-mairix-save-search' - save a mairix search from your history

;;; Code:
(require 'cl-lib)
(require 'cl-generic)
(require 'mairix)
(require 'ivy)
(require 'subr-x)
(require 'seq)


;; Custom stuff.
(defgroup counsel-mairix nil
  "Options for counsel-mairix."
  :group :mail
  :prefix "counsel-mairix-")

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

(defcustom counsel-mairix-include-threads 'prompt
  "Whether to prompt the user for including threads in the Mairix search.

If set to 'prompt, prompt the user.  If set to 'always, always
include threads.  If set to 'never, never prompt for threads."
  :type '(choice (const :tag "Prompt" prompt)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'counsel-mairix)


;; Generic methods that form the backbone of the search mechanism.
(cl-defgeneric counsel-mairix-run-search (frontend search-string threads)
  "Run Mairix with the search string SEARCH-STRING using FRONTEND.

Include threads in the result if THREADS is non-nil.")

(cl-defgeneric counsel-mairix-display-result-message (message)
  "Display MESSAGE using the right frontend.")

(defun counsel-mairix-determine-frontend ()
  "Try to compute the frontend that the user of Mairix is using."
  (or counsel-mairix-mail-frontend
      mairix-mail-program))

(defun counsel-mairix-search-file ()
  "Get the full path to the Mairix search file as given by `mairix-file-path' and `mairix-search-file'."
  (concat (file-name-as-directory (expand-file-name mairix-file-path))
          mairix-search-file))


;;; Rmail implementation of counsel-mairix.

(cl-defstruct counsel-mairix-rmail-result
  "A Mairix result entry to be displayed in Rmail."
  mbox-file msgnum)

(cl-defmethod counsel-mairix-run-search ((_ (eql rmail)) search-string threads)
  "Perform a Mairix search using SEARCH-STRING using Rmail.

If THREADS is non-nil, include threads."
  (require 'rmail)
  (let ((search-file (counsel-mairix-search-file))
        (large-file-warning-threshold nil)
        (rmail-display-summary t)
        sumbuf rmailbuf results)
    (progn
      (save-window-excursion
        (mairix-call-mairix search-string nil threads)
        ;; The search mbox might be open somewhere. Close it,
        ;; because its contents will change.
        (when-let ((searchbuffer (find-buffer-visiting search-file)))
          (kill-buffer searchbuffer))
        (rmail search-file)
        (with-current-buffer
            rmail-buffer
          (setq rmailbuf rmail-buffer)
          (setq sumbuf rmail-summary-buffer))
        (when (and rmailbuf sumbuf)
          (with-current-buffer sumbuf
            (font-lock-ensure)
            (setq results (split-string (buffer-string) "\n")))
          (kill-buffer rmailbuf)
          ;; The summary buffer might still be open. Kill it.
          (when sumbuf
            (kill-buffer sumbuf))))
      (mapcar
       (lambda (str)
         (when-let ((num (string-to-number (substring str 0 6)))
                    (res (make-counsel-mairix-rmail-result :msgnum num :mbox-file search-file)))
           ;; Counsel doesn't support rich results so we have to stuff things
           ;; into text properties.
           (propertize str 'result res)))
       (seq-remove #'string-empty-p results)))))

(cl-defmethod counsel-mairix-display-result-message ((result counsel-mairix-rmail-result))
  "Display RESULT using Rmail."
  (require 'rmail)
  (let ((large-file-warning-threshold nil))
    (rmail (counsel-mairix-rmail-result-mbox-file result))
    (rmail-show-message (counsel-mairix-rmail-result-msgnum result))))


;; Gnus implementation of the generic methods.
;; TODO...


;; VM implementation of the generic methods.
;; TODO...


;; The main implementation.

(defun counsel-mairix-do-search (str)
  "Either wait for more chars using `ivy-more-chars' or perform the search using STR after determining the correct search backend."
  (or (ivy-more-chars)
      (counsel-mairix-run-search (counsel-mairix-determine-frontend) str counsel-mairix-include-threads)
      '("" "Searching...")))

(cl-defmethod counsel-mairix-display-result-message ((result string))
  "Dispatch to `counsel-mairix-display-result-message' using the RESULT class stored in the 'result property of the search result, since the result class is stored there."
  (when-let (res (get-text-property 0 'result result))
    (counsel-mairix-display-result-message res)))

(defvar counsel-mairix-save-search-history ()
  "History for `counsel-mairix-save-search'.")

(defun counsel-mairix-save-search-action (search)
  "Save SEARCH as a Mairix search."
  (let ((mairix-last-search search))
    (mairix-save-search)))

(defvar counsel-mairix-history ()
  "History for `counsel-mairix'.")

(defun counsel-mairix-save-search ()
  "Save a search from the history of `counsel-mairix'.

If `counsel-mairix-history' is empty, save `mairix-last-search'."
  (interactive)
  (when (and (not counsel-mairix-history)
             (not mairix-last-search))
    (user-error "No counsel-mairix history or last mairix search to save from"))
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Save search: "
              (or (seq-reverse
                   (mapcar #'substring-no-properties
                           (seq-filter
                            (lambda (item)
                              (and (stringp item)
                                   (get-text-property 0 'ivy-index item)))
                            counsel-mairix-history)))
                  (list (car-safe mairix-last-search)))
              :require-match t
              :action #'counsel-mairix-save-search-action
              :caller 'counsel-mairix-save-search
              :history 'counsel-mairix-save-search-history)))

(defun counsel-mairix--get-field (field)
  "Return the header FIELD from the current message."
  (let ((get-mail-header
         (cadr (assq (counsel-mairix-determine-frontend)
                     mairix-get-mail-header-functions))))
    (if get-mail-header
        (mail-strip-quoted-names
         (funcall get-mail-header field))
      (error "No function for getting headers"))))

(defun counsel-mairix--ivy-yank-field (format field &optional process)
  "Use `with-ivy-window' to get FIELD from the current message.

The value of the field is formated using FORMAT and inserted into
the minibuffer.

If PROCESS is given, apply that function to the field value
before formating it."
  (let (from)
    (with-ivy-window
      (setq from (counsel-mairix--get-field field)))
    (when from
      (insert (format format (if process
                                 (funcall process from)
                               from))))))

(defun counsel-mairix-insert-from ()
  "Insert the `From:' field of a mail message into the minibuffer."
  (interactive)
  (counsel-mairix--ivy-yank-field "f:%s" "from"))

(defun counsel-mairix-insert-to ()
  "Insert the `Subject:' field of a mail message into the minibuffer."
  (interactive)
  (counsel-mairix--ivy-yank-field "t:%s" "to"))

(defun counsel-mairix-insert-subject ()
  "Insert the `Subject:' field of a mail message into the minibuffer."
  (interactive)
  (counsel-mairix--ivy-yank-field "s:%s" "subject"))

(defun counsel-mairix-insert-message-id ()
  "Insert the `Message-Id:' field of a mail message into the minibuffer."
  (interactive)
  (counsel-mairix--ivy-yank-field "m:%s" "message-id"))

(defun counsel-mairix-toggle-threads ()
  "Toggle threading on or off in the current search."
  (interactive)
  (let ((threads (if (eq 'prompt counsel-mairix-include-threads)
                     t
                   (not counsel-mairix-include-threads))))
    (setq-local counsel-mairix-include-threads threads)
    (ivy--reset-state ivy-last)))

(defun counsel-mairix-save-current-search ()
  "Save the current search, prompting for its name."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (prev mairix-last-search))
    (unwind-protect
        (let ((mairix-last-search ivy-text))
          (mairix-save-search))
      (setq prev mairix-last-search))))

(defun counsel-mairix-insert-saved-search ()
  "Insert a saved mairix search."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (searches (mapcar 'cdr mairix-saved-searches)))
    (insert (completing-read "Insert Mairix search: "
                             searches
                             nil t))))

(defvar counsel-mairix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f f") 'counsel-mairix-insert-from)
    (define-key map (kbd "C-c C-f t") 'counsel-mairix-insert-to)
    (define-key map (kbd "C-c C-f i") 'counsel-mairix-insert-message-id)
    (define-key map (kbd "C-c C-f s") 'counsel-mairix-insert-subject)
    (define-key map (kbd "C-c C-t")   'counsel-mairix-toggle-threads)
    (define-key map (kbd "C-c C-s s") 'counsel-mairix-save-current-search)
    (define-key map (kbd "C-c C-s i") 'counsel-mairix-insert-saved-search)
    map)
  "Keymap for `counsel-mairix'.")

;;;###autoload
(defun counsel-mairix (&optional initial-input)
  "Search using Mairix with an Counsel frontend.
It will determine the correct backend automatically based on the variable
`mairix-mail-program', this can be overridden using
`counsel-mairix-mail-frontend'.

'counsel-mairix' should support the same backends as mairix itself,
which are known to be Rmail (default), Gnus and VM.  Currently
only Rmail is supported.

If `counsel-mairix-include-threads' is nil, don't include threads
when searching with Mairix.  If it is t, always include
threads.  If it is prompt (the default), ask whether to include
threads or not.

If INITIAL-INPUT is given, the search has that as the initial input."
  (interactive)
  (let ((counsel-mairix-include-threads
         (if (eq 'prompt counsel-mairix-include-threads)
             (y-or-n-p "Include threads? ")
           counsel-mairix-include-threads))
        (enable-recursive-minibuffers t))
    (ivy-read "Mairix query: " #'counsel-mairix-do-search
              :dynamic-collection t
              :initial-input initial-input
              :action #'counsel-mairix-display-result-message
              :keymap counsel-mairix-map
              :history 'counsel-mairix-history
              :caller 'counsel-mairix)))

(defun counsel-mairix-search-from ()
  "Run `counsel-mairix' with the `From' header as the initial input."
  (interactive)
  (counsel-mairix
   (format "f:%s" (counsel-mairix--get-field "from"))))

(defun counsel-mairix-search-thread ()
  "Run `counsel-mairix' with the `Message-Id' header, with threading."
  (interactive)
  (let ((counsel-mairix-include-threads t))
    (counsel-mairix (format "m:%s" (counsel-mairix--get-field "message-id")))))

(provide 'counsel-mairix)

;;; counsel-mairix.el ends here
