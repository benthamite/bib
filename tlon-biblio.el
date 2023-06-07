;;; tlon-biblio.el --- A collection of convenience functions for bibliography management. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.0
;; Homepage: https://tlon.team
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

(require 'url)
(require 'url-http)
(require 'json)
(require 'seq)
(require 'zotra)

(defun tlon-biblio-get-doi-in-json (json-string)
  "Return DOI for selected candidate in JSON-STRING."
  (when-let* ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (data (json-read-from-string json-string))
              (items (alist-get 'items (alist-get 'message data)))
              (candidates (mapcar (lambda (item)
				    (let ((author-names (mapconcat
							 (lambda (author)
							   (concat (alist-get 'family author)
								   ", " (alist-get 'given author)))
							 (alist-get 'author item)
							 " & "))
					  (title (car (alist-get 'title item)))
					  (doi (alist-get 'DOI item)))
                                      (cons (concat author-names " - " title) doi)))
				  items))
              (selected-string (completing-read "Select a bibliographic entry: " candidates))
              (selected-doi (cdr (assoc selected-string candidates))))
    selected-doi))

(defun tlon-biblio--query-crossref (title &optional author)
  "Query the Crossref database for TITLE and AUTHOR."
  (let* ((url-request-method "GET")
         (url (concat (format "https://api.crossref.org/works?query.bibliographic=%s"
			      (url-hexify-string title))
		      (when author
			(format "&query.author=%s"
				(url-hexify-string author)))))
         (url-buffer (url-retrieve-synchronously url))
         (json-string (with-current-buffer url-buffer
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (buffer-substring-no-properties (point) (point-max)))))
    (tlon-biblio-get-doi-in-json json-string)))

(defun tlon-biblio-zotra-add-entry-from-title ()
  "Prompt user for title and author and add selection to bibfile via its associated DOI."
  (interactive)
  (let ((title (read-string "Enter title: "))
        (author (read-string "Enter author: ")))
    (if-let ((doi (tlon-biblio--query-crossref title
					       (unless (string= "" author)
						 author))))
	(zotra-add-entry-from-search doi)
      (message "No DOI entries found for the given title and author."))))

(provide 'tlon-biblio)
;;; tlon-biblio.el ends here
