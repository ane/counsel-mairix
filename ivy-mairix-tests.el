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

;; tests for ivy-mairix.

(ert-deftest test-mairix-presence ()
  "Test whether mairix is installed on the system."
  (not (null (executable-find "mairix"))))

(ert-deftest test-packages ()
  "Test whether ivy and mairix are installed."
  (and (featurep 'ivy)
       (featurep 'mairix)))


