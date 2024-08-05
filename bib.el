;;; bib.el --- Rudimentary support for bibliographic information retrieval. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.0
;; Homepage: https://github.com/benthamite/bib


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

;; This package provides rudimentary support for retrieving bibliographic
;; information for books, papers and films. By searching for title and/or
;; author, the package determines the associated unique identifier (an ISBN, a
;; DOI, or an IMDB ID, respectively), which is then used to retrieve the
;; relevant information and generate the corresponding bibtex entry.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'seq)
(require 'zotra)

;;;; User options

(defgroup bib ()
  "Rudimentary support for bibliographic information retrieval."
  :group 'bibtex)

(defcustom bib-downloads-dir (expand-file-name "~/Downloads/")
  "Directory where downloaded files are stored."
  :type 'directory
  :group 'bib)

(defcustom bib-imdb-use-mullvad-p nil
  "If non-nil, connect to this city temporarily to fetch film data from IMDb.
The Internet Movie Database fetches data in the language of the country where
the user is located. If you want to fetch data in a different language, set this
user option to a city where this language is spoken.

This option requires the `mullvad' package, and the user options
`mullvad-websites-and-cities' and `mullvad-cities-and-servers' to be set
accordingly. Specifically, `mullvad-websites-and-cities' must include a cons
cell whose car is `\"IMDb\"' and whose cdr is the city you want `mullvad' to
connect to (e.g. `(\"IMDb\" . \"New York\")'), and `mullvad-cities-and-servers'
must include a cons cell whose car is this city and whose cdr is a Mullvad
server for this city (e.g. `(\"New York\" . \"us-nyc-wg-601\")'). Here is a
sample configuration:

\(setq mullvad-cities-and-servers
      '((\"London\" . \"gb-lon-wg-001\")
        (\"Madrid\" . \"es-mad-wg-101\")
        (\"Malmö\" . \"se-sto-wg-001\")
        (\"Frankfurt\" . \"de-fra-wg-001\")
        (\"New York\" . \"us-nyc-wg-601\")
        (\"San José\" . \"us-sjc-wg-001\")
        (\"São Paulo\" . \"br-sao-wg-001\")))

\(setq mullvad-websites-and-cities
      '((\"Betfair\" . \"London\")
        (\"Criterion Channel\" . \"New York\")
        (\"Gemini\" . \"New York\")
        (\"HathiTrust\" . \"San José\")
        (\"IMDb\" . \"New York\")
        (\"Library Genesis\" . \"Malmö\")
        (\"Pirate Bay\" . \"Malmö\")
        (\"UC Berkeley\" . \"San José\")
        (\"Wise\" . \"Madrid\")))

Refer to the `mullvad' package documentation for details."
  :type 'boolean
  :group 'bib)

;;;;; API keys

(defcustom bib-isbndb-key ""
  "Private key for the `ISBNdb' database."
  :type 'string
  :group 'bib)

(defcustom bib-omdb-key ""
  "Private key for The Open Movie Database."
  :type 'string
  :group 'bib)

(defcustom bib-tmdb-key ""
  "Private key for The Movie Database.
This key is only used to translate the title of a film into English."
  :type 'string
  :group 'bib)

;;;; Functions

(defun bib-reverse-first-last-name (author)
  "Reverse the order of comma-separated elements in AUTHOR field."
  (replace-regexp-in-string "\\(.*\\), \\(.*\\)" "\\2 \\1" author))

(defun bib-get-doi-in-json (json-string)
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
				      (cons (concat title " by " (bib-reverse-first-last-name author-names)) doi)))
				  items))
	      (selected-string (completing-read "Select a bibliographic entry: " candidates))
	      (selected-doi (cdr (assoc selected-string candidates))))
    selected-doi))

(defun bib-search-crossref (&optional title author)
  "Query the Crossref database for TITLE and AUTHOR."
  (let* ((title (or title (read-from-minibuffer "Enter title: ")))
	 (author (or author (read-from-minibuffer "Enter author (optional): ")))
	 (url-request-method "GET")
	 (url (concat (format "https://api.crossref.org/works?query.bibliographic=%s"
			      (url-hexify-string title))
		      (when (and author
				 (not (string-empty-p author)))
			(format "&query.author=%s"
				(url-hexify-string author)))))
	 (url-buffer nil)
	 (json-string nil))
    (setq url-buffer (url-retrieve-synchronously url))
    (setq json-string (with-current-buffer url-buffer
			(goto-char (point-min))
			(re-search-forward "^$")
			(buffer-substring-no-properties (point) (point-max))))
    (when json-string (message (bib-get-doi-in-json json-string)))))

(defun bib-fetch-abstract-from-crossref (doi)
  "Return the abstract of the work with DOI."
  (let ((url (format "https://api.crossref.org/works/%s" doi)))
    (message "Trying to find abstract for %s with `crossref'..." doi)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (if (search-forward-regexp "HTTP/.* 404" nil t) ; check for 404 not found
          (progn
            (kill-buffer)
            nil)
        (re-search-forward "^$")
        (delete-region (point) (point-min))
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               (json (json-read))
               (message-plist (plist-get json :message)))
	  (kill-buffer)
          (when-let ((abstract (plist-get message-plist :abstract)))
	    abstract))))))

;;;; ISBN

(defun bib-search-isbn (&optional query)
  "Query the ISBNdb database for QUERY.
The query may include the title, author, or ISBN of the book."
  (interactive)
  (let* ((query (or query (read-string "Enter query (title and/or author): ")))
	 (url (format "https://api2.isbndb.com/books/%s?page=1&pageSize=20" (url-hexify-string query)))
	 (url-request-method "GET")
	 (url-request-extra-headers
	  `(("Accept" . "application/json")
	    ("Authorization" . ,bib-isbndb-key)))
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
						     (bib-reverse-first-last-name (car authors)))
					   (format "%s, " title))
					 isbn)))
			       result-list))
	   (selection (completing-read "Select a book: " candidates)))
      (cdr (assoc selection candidates)))))

(defun bib-fetch-abstract-from-google-books (isbn)
  "Return the abstract of the book with ISBN."
  (let ((url (format "https://www.googleapis.com/books/v1/volumes?q=isbn:%s" isbn))
        (description nil))
    (message "Trying to find abstract for %s with `Google Books'..." isbn)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (let* ((json-object-type 'plist)
             (json-array-type 'list)
             (json (json-read))
             (items (plist-get json :items))
             (volume-info (and items (plist-get (car items) :volumeInfo))))
        (setq description (and volume-info (plist-get volume-info :description)))))
    (when (get-buffer url)
      (kill-buffer url))
    description))


;;;; imdb

(declare-function mullvad-connect-to-website "mullvad")
(defun bib-search-imdb (&optional title)
  "Prompt user for TITLE, then add film to bibfile via its IMDb ID.
For this command to work, you must set `bib-omdb-key' to a valid OMDb API key.
You can get a free key at <http://www.omdbapi.com/>."
  (interactive)
  (when bib-imdb-use-mullvad-p
    (mullvad-connect-to-website "IMDb" 1 'silently))
  (let* ((title (or title (read-from-minibuffer "Enter movie title: ")))
	 (url (format
	       "https://www.omdbapi.com/?s=%s&apikey=%s"
	       (replace-regexp-in-string " " "+" title) bib-omdb-key)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n")
      (let* ((json-object-type 'plist)
	     (json (json-read))
	     (movies (plist-get json :Search)))
	(kill-buffer)
	(if movies
	    (let* ((candidates (mapcar (lambda (movie)
					 (cons (format "%s (%s)"
						       (plist-get movie :Title)
						       (plist-get movie :Year))
					       (plist-get movie :imdbID)))
				       movies))
		   (movie (assoc (completing-read "Select a movie: " candidates) candidates)))
	      (concat "https://www.imdb.com/title/" (cdr movie)))
	  (user-error "No matching movies found"))))))

(defvar url-http-end-of-headers)
(defun bib-translate-title-into-english (title)
  "Return English title of TITLE.
If TITLE is itself an English title, return it unchanged."
  (let* ((search-url (format
		      "https://api.themoviedb.org/3/search/movie?api_key=%s&query=%s"
		      bib-tmdb-key (url-hexify-string title))))
    (with-current-buffer (url-retrieve-synchronously search-url)
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
	     (results (cdr (assoc 'results response))) ; Extract results vector
	     (first-result (elt results 0)) ; Get the first movie
	     (english-title (cdr (assoc 'title first-result)))) ; Extract the title
	english-title))))

(declare-function zotra-extras-add-entry "zotra-extras")
(defun bib-zotra-add-entry-from-title ()
  "Add bibliography entry from its title."
  (interactive)
  (require 'zotra)
  (let* ((type (completing-read "Type of search:" '("doi" "isbn" "imdb") nil t))
	 (id (pcase type
	       ("doi" (bib-search-crossref))
	       ("isbn" (bib-search-isbn))
	       ("imdb" (bib-search-imdb)))))
    (zotra-extras-add-entry id)))

(defun bib-libgen (query)
  "Search for QUERY in Library Genesis."
  (interactive "sQuery: ")
  (let ((app "libby"))
    (unless (executable-find app)
      (user-error "Please install %s (https://github.com/carterprince/libby)" app))
    (term (format "%s '%s' --no-view --output-dir %s --lang spa" app query bib-downloads-dir))))

(provide 'bib)

;;; bib.el ends here
