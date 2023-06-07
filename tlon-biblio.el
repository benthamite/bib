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
                                      (cons (concat title " by " (tlon-biblio-reverse-first-last-name author-names)) doi)))
				  items))
              (selected-string (completing-read "Select a bibliographic entry: " candidates))
              (selected-doi (cdr (assoc selected-string candidates))))
    selected-doi))

(defun tlon-biblio-reverse-first-last-name (author)
  "Reverse the order of comma-separated elements in AUTHOR field."
  (replace-regexp-in-string "\\(.*\\), \\(.*\\)" "\\2 \\1" author))

(defun tlon-biblio-search-crossref (title &optional author)
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
    (message (tlon-biblio-get-doi-in-json json-string))))

(defvar tlon-biblio-isbndb-key
  (auth-source-pass-get "key" (concat "tlon/BAE/isbndb.com/" ps/tlon-email)))

(defun tlon-biblio-search-isbndb (title)
  "Query the ISBNdb database for TITLE."
  (let* ((url (format "https://api2.isbndb.com/books/%s?page=1&pageSize=20" (url-hexify-string title)))
         (url-request-method "GET")
         (url-request-extra-headers
	  `(("Accept" . "application/json")
	    ("Authorization" . ,tlon-biblio-isbndb-key)))
         (url-buffer (url-retrieve-synchronously url))
         (json-object-type 'plist)
         (json-key-type 'keyword)
         (json-array-type 'list)
         json-data
         result-list)
    (with-current-buffer url-buffer
      (goto-char (point-min))
      (search-forward "\n\n") ;; Skip response headers
      (setq json-data (json-read)))
    (setq result-list (plist-get json-data :books))
    (unless result-list
      (error "No results found"))
    (let* ((candidates (mapcar (lambda (book)
                                 (cons (format "%s by %s" 
                                               (plist-get book :title) 
					       (tlon-biblio-reverse-first-last-name (car (plist-get book :authors)))
					       ", ")
				       (plist-get book :isbn)))
			       result-list))
	   (selection (completing-read "Select a book: " candidates)))
      (cdr (assoc selection candidates)))))

(defun tlon-biblio-zotra-add-entry-from-title ()
  "Prompt user for title and author and add selection to bibfile via its associated DOI."
  (interactive)
  (let ((type (completing-read "DOI or ISBN? " '("doi" "isbn") nil t))
	(title (read-string "Enter title: "))
	author)
    (when (string= type "doi")
      (setq author (read-string "Enter author: ")))
    (if-let (identifier (if (string= type "doi")
			    (tlon-biblio-search-crossref title (unless (string= author "") author))
			  (tlon-biblio-search-isbndb title)))
	(zotra-add-entry-from-search identifier)
      (user-error "No entries found"))))

(provide 'tlon-biblio)
;;; tlon-biblio.el ends here
