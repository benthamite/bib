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

(defvar tlon-biblio-version "0.1.0")

(defun tlon-biblio-reverse-first-last-name (author)
  "Reverse the order of comma-separated elements in AUTHOR field."
  (replace-regexp-in-string "\\(.*\\), \\(.*\\)" "\\2 \\1" author))

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

(defun tlon-biblio-search-crossref (title &optional author)
  "Query the Crossref database for TITLE and AUTHOR."
  (let* ((url-request-method "GET")
	 (url (concat (format "https://api.crossref.org/works?query.bibliographic=%s"
			      (url-hexify-string title))
		      (when (and author
				 (not (string-empty-p author)))
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
      (user-error "No results found"))
    (let* ((candidates (mapcar (lambda (book)
				 (let* ((title (plist-get book :title))
					(authors (plist-get book :authors))
					(isbn (plist-get book :isbn)))
				   (cons (if authors
					     (format "%s by %s"
						     title
						     (tlon-biblio-reverse-first-last-name (car authors))
						     ", ")
					   (format "%s, " title))
					 isbn)))
			       result-list))
	   (selection (completing-read "Select a book: " candidates)))
      (cdr (assoc selection candidates)))))

(defvar tlon-biblio-imdb-key (auth-source-pass-get "key" (concat "chrome/imdb-api.com/" ps/personal-email)))

(defun tlon-biblio-search-imdb (title &optional year)
  "Prompt user for title and optional year and add selection to bibfile via its IMDb ID."
  (let* ((encoded-title (url-hexify-string title))
         (url (format "https://imdb-api.com/en/API/SearchMovie/%s/%s" tlon-biblio-imdb-key encoded-title))
         (response (request url
                     :sync t
                     :type "GET"
                     :parser 'json-read
                     :log-level 'warn
                     :data nil))
         (status (request-response-status-code response))
         (data (request-response-data response))
         (results (when data (cdr (assoc 'results data))))
         (items (when results
                  (cl-loop for result across results
                           for id = (cdr (assoc 'id result))
                           for result-title = (cdr (assoc 'title result))
                           for desc = (cdr (assoc 'description result))
                           for result-year = (progn
					       (string-match ".*?\\([[:digit:]]\\{4\\}\\)" desc)
					       (match-string 1 desc))
                           when (or (string-empty-p year) (string= result-year year))
                           collect (cons (format "%s (%s)" result-title result-year) id))))
         (choice (when items (completing-read "Select a movie: " items)))
         (imdb-id (cdr (assoc choice items))))
    (or imdb-id
	(message "No match found for %s (%s)" title (if (string= year "") "Any year" year)))))

(defun tlon-biblio-zotra-add-entry-from-title ()
  "Prompt user for title and author and add selection to bibfile via its identifier."
  (interactive)
  (let* ((type (completing-read "Type of search:" '("doi" "isbn" "imdb") nil t))
	 (title (read-string "title: "))
	 (field (pcase type
		  ((or "doi" "isbn") "author ")
		  ("imdb" "year ")))
	 (extra (read-string (concat field "(optional): ")))
	 (fun (pcase type
		((or "doi" "isbn") 'tlon-biblio-search-crossref)
		("imdb" 'tlon-biblio-search-imdb))))
    (if-let (id (funcall fun title extra))
	(pcase type
	  ((or "doi" "isbn") (zotra-add-entry-from-search id))
	  ("imdb" (zotra-add-entry-from-url (concat "https://www.imdb.com/title/" id))))
      (user-error "No entries found"))))

(provide 'tlon-biblio)
;;; tlon-biblio.el ends here

