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
(require 'url-util)
(require 'json)

;;;; Variables

(defconst bib-letterboxd-url "https://letterboxd.com/film/%s/"
  "URL template for Letterboxd film pages.")

;;;; User options

(defgroup bib ()
  "Rudimentary support for bibliographic information retrieval."
  :group 'bibtex)

(defcustom bib-letterboxd-use-slug-p nil
  "Whether to return the Letterboxd slug instead of the full URL.
When non-nil, `bib-search-letterboxd' returns the slug by default;
when nil, it returns the full URL.  A prefix argument inverts the
behavior for a single invocation."
  :type 'boolean
  :group 'bib)

(define-obsolete-variable-alias 'letterboxd-user-agent
  'bib-letterboxd-user-agent "0.1.1")

(defcustom bib-letterboxd-user-agent
  "Mozilla/5.0 (compatible; Emacs bib.el)"
  "User-Agent string sent when querying Letterboxd."
  :type 'string
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

(make-obsolete-variable 'bib-imdb-use-mullvad-p nil "2024-09-26")

;;;; Functions

;;;;; Helpers

(defun bib--ensure-key (key service)
  "Signal a `user-error' if KEY is nil or empty, naming SERVICE."
  (when (or (null key) (string-empty-p key))
    (user-error "Please set `bib-%s-key' before using this command" service)))

(defun bib--http-get (url &optional extra-headers)
  "Fetch URL synchronously and return the response body as a string.
EXTRA-HEADERS is an alist of additional HTTP headers.
Returns nil if the request fails."
  (let ((url-request-method "GET")
        (url-request-extra-headers extra-headers))
    (pcase (url-retrieve-synchronously url t nil 15)
      ('nil nil)
      (buf (unwind-protect
               (with-current-buffer buf
                 (goto-char (point-min))
                 (re-search-forward "\r?\n\r?\n" nil 'move)
                 (buffer-substring-no-properties (point) (point-max)))
             (when (buffer-live-p buf)
               (kill-buffer buf)))))))

(defun bib--parse-json (text)
  "Parse JSON TEXT into an alist with symbol keys, or nil on error."
  (when (and text (not (string-empty-p text)))
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-from-string text))
      (json-error nil))))

(defun bib--completing-read (prompt candidates)
  "Let the user choose from CANDIDATES using PROMPT.
CANDIDATES is an alist of (DISPLAY . VALUE).  Returns the VALUE
of the selected entry."
  (let ((selection (completing-read prompt candidates nil t)))
    (cdr (assoc selection candidates))))

;;;;; Crossref

(defun bib-search-crossref (&optional title author)
  "Query the Crossref database for TITLE and AUTHOR.
Return the DOI of the selected candidate."
  (let* ((title (or title (read-from-minibuffer "Enter title: ")))
	 (author (or author (read-from-minibuffer "Enter author (optional): ")))
	 (url (concat (format "https://api.crossref.org/works?query.bibliographic=%s"
			      (url-hexify-string title))
		      (when (and author (not (string-empty-p author)))
			(format "&query.author=%s" (url-hexify-string author)))))
	 (body (or (bib--http-get url)
		   (error "Network request to Crossref failed")))
	 (data (bib--parse-json body)))
    (when data
      (let ((doi (bib--crossref-select-doi data)))
	(when doi (message "%s" doi))
	doi))))

(defun bib-fetch-abstract-from-crossref (doi)
  "Return the abstract of the work with DOI."
  (message "Trying to find abstract for %s with `crossref'..." doi)
  (let* ((url (format "https://api.crossref.org/works/%s" (url-hexify-string doi)))
	 (data (bib--parse-json (bib--http-get url)))
	 (msg (and data (alist-get 'message data))))
    (when (consp msg)
      (alist-get 'abstract msg))))

(defun bib--crossref-select-doi (data)
  "Let the user select a DOI from Crossref response DATA."
  (when-let* ((items (alist-get 'items (alist-get 'message data)))
	      (candidates (mapcar (lambda (item)
				    (let* ((authors (alist-get 'author item))
					   (author-names
					    (when authors
					      (mapconcat
					       (lambda (a)
						 (concat (alist-get 'family a)
							 ", " (alist-get 'given a)))
					       authors " & ")))
					   (title (or (car (alist-get 'title item))
						      "[untitled]"))
					   (doi (alist-get 'DOI item))
					   (display (if (and author-names
							     (not (string-empty-p author-names)))
							(concat title " by "
								(bib-reverse-first-last-name author-names))
						      title)))
				      (cons display doi)))
				  items)))
    (bib--completing-read "Select a bibliographic entry: " candidates)))

;;;###autoload
(defun bib-reverse-first-last-name (author)
  "Reverse the order of comma-separated elements in AUTHOR field.
Handles multiple authors separated by \" & \" correctly."
  (mapconcat
   (lambda (name)
     (replace-regexp-in-string
      "\\`\\(.*\\), \\(.*\\)\\'" "\\2 \\1" (string-trim name)))
   (split-string author " & ")
   " & "))

;;;;; ISBN

(defun bib-search-isbn (&optional query)
  "Query the ISBNdb database for QUERY.
The query may include the title, author, or ISBN of the book."
  (interactive)
  (bib--ensure-key bib-isbndb-key "isbndb")
  (let* ((query (or query (read-string "Enter query (title and/or author): ")))
	 (url (format "https://api2.isbndb.com/books/%s?page=1&pageSize=20"
		      (url-hexify-string query)))
	 (body (or (bib--http-get url `(("Accept" . "application/json")
					("Authorization" . ,bib-isbndb-key)))
		   (error "Network request to ISBNdb failed")))
	 (data (bib--parse-json body))
	 (books (alist-get 'books data)))
    (unless books
      (user-error "No results found"))
    (let ((candidates (mapcar (lambda (book)
				(let* ((title (alist-get 'title book))
				       (author (car-safe (alist-get 'authors book)))
				       (isbn (alist-get 'isbn book)))
				  (cons (if (and author (stringp author))
					    (format "%s by %s" title
						    (bib-reverse-first-last-name author))
					  title)
					isbn)))
			      books)))
      (bib--completing-read "Select a book: " candidates))))

(defun bib-fetch-abstract-from-google-books (isbn)
  "Return the abstract of the book with ISBN."
  (message "Trying to find abstract for %s with `Google Books'..." isbn)
  (let* ((url (format "https://www.googleapis.com/books/v1/volumes?q=isbn:%s"
		      (url-hexify-string isbn)))
	 (data (bib--parse-json (bib--http-get url)))
	 (items (and data (alist-get 'items data)))
	 (volume-info (and items (alist-get 'volumeInfo (car items)))))
    (and volume-info (alist-get 'description volume-info))))

;;;;; IMDb

(defun bib-search-imdb (&optional title)
  "Prompt user for TITLE, then add film to bibfile via its IMDb ID.
For this command to work, you must set `bib-omdb-key' to a valid OMDb API key.
You can get a free key at <http://www.omdbapi.com/>."
  (interactive)
  (bib--ensure-key bib-omdb-key "omdb")
  (let* ((title (or title (read-from-minibuffer "Enter movie title: ")))
	 (url (format "https://www.omdbapi.com/?s=%s&apikey=%s"
		      (url-hexify-string title) bib-omdb-key))
	 (body (or (bib--http-get url)
		   (error "Network request to OMDb failed")))
	 (data (bib--parse-json body))
	 (movies (and data (alist-get 'Search data))))
    (if movies
	(let ((candidates (mapcar (lambda (movie)
				    (cons (format "%s (%s)"
						  (alist-get 'Title movie)
						  (alist-get 'Year movie))
					  (alist-get 'imdbID movie)))
				  movies)))
	  (concat "https://www.imdb.com/title/"
		  (bib--completing-read "Select a movie: " candidates)))
      (user-error "No matching movies found"))))

(defun bib-translate-title-into-english (title)
  "Return English title of TITLE.
If TITLE is itself an English title, return it unchanged.
Requires `bib-tmdb-key' to be set."
  (bib--ensure-key bib-tmdb-key "tmdb")
  (let* ((url (format "https://api.themoviedb.org/3/search/movie?api_key=%s&query=%s"
		      bib-tmdb-key (url-hexify-string title)))
	 (body (or (bib--http-get url)
		   (error "Network request to TMDb failed")))
	 (data (bib--parse-json body))
	 (results (and data (alist-get 'results data))))
    (if (and results (> (length results) 0))
	(alist-get 'title (car results))
      (user-error "No results found for \"%s\"" title))))

(declare-function zotra-extras-add-entry "zotra-extras")
(defun bib-zotra-add-entry-from-title ()
  "Add bibliography entry from its title."
  (interactive)
  (require 'zotra)
  (let* ((type (completing-read "Type of search: " '("doi" "isbn" "imdb" "letterboxd") nil t))
	 (id (pcase type
	       ("doi" (bib-search-crossref))
	       ("isbn" (bib-search-isbn))
	       ("imdb" (bib-search-imdb))
	       ("letterboxd" (bib-search-letterboxd nil t)))))
    (zotra-extras-add-entry id)))

;;;;; Letterboxd

;;;###autoload
(defun bib-search-letterboxd (&optional query full-url)
  "Prompt for QUERY, let you choose a film, and return its slug or URL.
When called interactively, also copy the result to the kill ring.

By default, the return format is controlled by `bib-letterboxd-use-slug-p':
if non-nil, the slug is returned; otherwise the full URL.  With a prefix
argument, the behavior is inverted.

When FULL-URL is non-nil in a Lisp call, always return the full URL
regardless of `bib-letterboxd-use-slug-p'."
  (interactive)
  (let* ((query (or query (read-string "Search Letterboxd for film: ")))
	 (return-slug (and (not full-url)
			   (if (and (called-interactively-p 'any) current-prefix-arg)
			       (not bib-letterboxd-use-slug-p)
			     bib-letterboxd-use-slug-p)))
	 (items  (bib-lbx--fetch-items query))
         (slug   (bib--completing-read "Choose film: " items))
         (text   (if return-slug slug (format bib-letterboxd-url slug))))
    (when (called-interactively-p 'interactive)
      (kill-new text)
      (message "%s" text))
    text))

(defun bib-lbx--http-get (url &optional accept)
  "Return body of URL as string or nil on network error.
If ACCEPT is given, send it as the Accept header."
  (bib--http-get url
		 `(("User-Agent"       . ,bib-letterboxd-user-agent)
		   ("X-Requested-With" . "XMLHttpRequest")
		   ("Referer"          . "https://letterboxd.com/")
		   ,@(when accept `(("Accept" . ,accept))))))

(defun bib-lbx--items-from-json (alist)
  "Return a list of (DISPLAY . SLUG) pairs parsed from Letterboxd ALIST.
ALIST is the decoded JSON object returned by `bib--parse-json'."
  (mapcar
   (lambda (it)
     (let* ((title (alist-get 'name it))
            (year  (alist-get 'year it))
            (url   (or (alist-get 'url it) ""))
            (slug  (replace-regexp-in-string "\\`/film/\\|/\\'" "" url))
            (disp  (if year (format "%s (%s)" title year) title)))
       (cons disp slug)))
   (alist-get 'items alist)))

(defun bib-lbx--json-items (query)
  "Return a list of (DISPLAY . SLUG) pairs for QUERY via Letterboxd JSON.
QUERY is the user-supplied search string.  The function tries two
Letterboxd autocomplete endpoints and returns nil when both fail."
  (let* ((enc (url-hexify-string query))
         (json
          (or (bib--parse-json
	       (bib-lbx--http-get
                (format "https://letterboxd.com/s/autocompletefilm/?input=%s"
                        enc) "application/json"))
	      (bib--parse-json
	       (bib-lbx--http-get
                (format
                 "https://letterboxd.com/search/autocomplete/?q=%s&type=film"
                 enc) "application/json")))))
    (when (and json (alist-get 'items json))
      (bib-lbx--items-from-json json))))

(defun bib-lbx--ddg-items (query)
  "Return (DISPLAY . SLUG) list for QUERY via DuckDuckGo site: search.
QUERY is the user-supplied search string.  DuckDuckGo HTML search is
used as a fallback when the Letterboxd JSON endpoint is unavailable."
  (let* ((enc  (url-hexify-string
                (concat "site:letterboxd.com/film " query)))
         (html (bib-lbx--http-get
                (format "https://duckduckgo.com/html/?q=%s" enc)))
         ;; Decode twice: some results are double-encoded, so a second
         ;; pass makes sure every %2F becomes "/", etc.
         (decoded (and html (url-unhex-string html)))
         (decoded (and decoded (url-unhex-string decoded)))
         (pos 0)  seen out)
    (while (and decoded
                (string-match
                 "letterboxd\\.com/film/\\([a-z0-9-]+\\)/" decoded pos)
                (< (length out) 40))
      (setq pos (match-end 0))
      (let ((slug (match-string 1 decoded)))
        (unless (member slug seen)
          (push slug seen)
          (let ((disp (capitalize (replace-regexp-in-string "-" " " slug))))
            (push (cons disp slug) out)))))
    (nreverse out)))

(defun bib-lbx--fetch-items (query)
  "Return (DISPLAY . SLUG) list for QUERY using Letterboxd or DuckDuckGo.
QUERY is the search string.  The JSON endpoint is tried first, falling
back to DuckDuckGo when it returns nil.  The function signals an error
when no results can be obtained."
  (or (bib-lbx--json-items query)
      (progn
        (message "[letterboxd] JSON unavailable -- using DuckDuckGo")
        (let ((items (bib-lbx--ddg-items query)))
          (unless items
            (error "Letterboxd: no results for \"%s\"" query))
          items))))

(provide 'bib)

;;; bib.el ends here
