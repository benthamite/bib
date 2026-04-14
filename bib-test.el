;;; bib-test.el --- Tests for bib.el. -*- lexical-binding: t -*-

;;; Commentary:

;; ERT test suite for the `bib' package.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bib)

;;;; bib-reverse-first-last-name

(ert-deftest bib-test-reverse-first-last-name-single ()
  "Reverse a single \"Last, First\" author."
  (should (equal (bib-reverse-first-last-name "Smith, John")
                 "John Smith")))

(ert-deftest bib-test-reverse-first-last-name-multiple ()
  "Reverse multiple authors separated by \" & \"."
  (should (equal (bib-reverse-first-last-name "Smith, John & Doe, Jane")
                 "John Smith & Jane Doe")))

(ert-deftest bib-test-reverse-first-last-name-no-comma ()
  "Return the name unchanged when there is no comma."
  (should (equal (bib-reverse-first-last-name "John Smith")
                 "John Smith")))

(ert-deftest bib-test-reverse-first-last-name-whitespace ()
  "Strip leading and trailing whitespace from each name."
  (should (equal (bib-reverse-first-last-name "  Smith, John  ")
                 "John Smith")))

(ert-deftest bib-test-reverse-first-last-name-empty ()
  "Handle an empty string gracefully."
  (should (equal (bib-reverse-first-last-name "") "")))

;;;; bib--parse-json

(ert-deftest bib-test-parse-json-valid ()
  "Parse a valid JSON string into an alist."
  (let ((result (bib--parse-json "{\"key\": \"value\"}")))
    (should (equal (alist-get 'key result) "value"))))

(ert-deftest bib-test-parse-json-array ()
  "Parse a JSON array into a list."
  (let ((result (bib--parse-json "[1, 2, 3]")))
    (should (equal result '(1 2 3)))))

(ert-deftest bib-test-parse-json-nil ()
  "Return nil when input is nil."
  (should (null (bib--parse-json nil))))

(ert-deftest bib-test-parse-json-empty-string ()
  "Return nil when input is an empty string."
  (should (null (bib--parse-json ""))))

(ert-deftest bib-test-parse-json-malformed ()
  "Return nil on malformed JSON instead of signaling an error."
  (should (null (bib--parse-json "{not valid json}"))))

(ert-deftest bib-test-parse-json-nested ()
  "Parse nested JSON objects correctly."
  (let ((result (bib--parse-json "{\"a\": {\"b\": 42}}")))
    (should (equal (alist-get 'b (alist-get 'a result)) 42))))

;;;; bib--ensure-key

(ert-deftest bib-test-ensure-key-nil ()
  "Signal `user-error' when key is nil."
  (should-error (bib--ensure-key nil "test") :type 'user-error))

(ert-deftest bib-test-ensure-key-empty ()
  "Signal `user-error' when key is an empty string."
  (should-error (bib--ensure-key "" "test") :type 'user-error))

(ert-deftest bib-test-ensure-key-valid ()
  "Do not signal when key is a non-empty string."
  (should (null (bib--ensure-key "abc123" "test"))))

;;;; bib--open-library-url

(ert-deftest bib-test-open-library-url-basic ()
  "Build a correctly encoded Open Library URL."
  (let ((url (bib--open-library-url "test query")))
    (should (string-match-p "openlibrary\\.org/search\\.json" url))
    (should (string-match-p "test%20query" url))))

;;;; bib--open-library-isbn

(ert-deftest bib-test-open-library-isbn-prefers-isbn13 ()
  "Prefer an ISBN-13 over a shorter ISBN."
  (let ((doc '((isbn . ("1234567890" "1234567890123")))))
    (should (equal (bib--open-library-isbn doc) "1234567890123"))))

(ert-deftest bib-test-open-library-isbn-falls-back-to-first ()
  "Fall back to the first ISBN when no ISBN-13 is present."
  (let ((doc '((isbn . ("1234567890" "0987654321")))))
    (should (equal (bib--open-library-isbn doc) "1234567890"))))

(ert-deftest bib-test-open-library-isbn-nil-when-missing ()
  "Return nil when doc has no isbn field."
  (let ((doc '((title . "A Book"))))
    (should (null (bib--open-library-isbn doc)))))

(ert-deftest bib-test-open-library-isbn-nil-when-empty ()
  "Return nil when isbn list is empty."
  (let ((doc '((isbn))))
    (should (null (bib--open-library-isbn doc)))))

;;;; bib--open-library-full-title

(ert-deftest bib-test-open-library-full-title-with-subtitle ()
  "Concatenate title and subtitle with a colon."
  (let ((doc '((title . "Main") (subtitle . "Sub"))))
    (should (equal (bib--open-library-full-title doc) "Main: Sub"))))

(ert-deftest bib-test-open-library-full-title-without-subtitle ()
  "Return only the title when subtitle is absent."
  (let ((doc '((title . "Main"))))
    (should (equal (bib--open-library-full-title doc) "Main"))))

(ert-deftest bib-test-open-library-full-title-empty-subtitle ()
  "Return only the title when subtitle is an empty string."
  (let ((doc '((title . "Main") (subtitle . ""))))
    (should (equal (bib--open-library-full-title doc) "Main"))))

;;;; bib--open-library-display

(ert-deftest bib-test-open-library-display-full ()
  "Format display string with author and year."
  (let ((doc '((title . "Book") (author_name . ("Author")) (first_publish_year . 2020))))
    (should (equal (bib--open-library-display doc) "Book by Author (2020)"))))

(ert-deftest bib-test-open-library-display-no-author ()
  "Format display string without author."
  (let ((doc '((title . "Book") (first_publish_year . 2020))))
    (should (equal (bib--open-library-display doc) "Book (2020)"))))

(ert-deftest bib-test-open-library-display-no-year ()
  "Format display string without year."
  (let ((doc '((title . "Book") (author_name . ("Author")))))
    (should (equal (bib--open-library-display doc) "Book by Author"))))

(ert-deftest bib-test-open-library-display-minimal ()
  "Format display string with only title."
  (let ((doc '((title . "Book"))))
    (should (equal (bib--open-library-display doc) "Book"))))

;;;; bib--open-library-candidate

(ert-deftest bib-test-open-library-candidate-with-isbn ()
  "Return a cons cell when doc has an ISBN."
  (let ((doc '((title . "Book") (isbn . ("1234567890123")))))
    (should (consp (bib--open-library-candidate doc)))))

(ert-deftest bib-test-open-library-candidate-without-isbn ()
  "Return nil when doc has no ISBN."
  (let ((doc '((title . "Book"))))
    (should (null (bib--open-library-candidate doc)))))

;;;; bib--open-library-candidates

(ert-deftest bib-test-open-library-candidates-filters-no-isbn ()
  "Filter out documents that lack an ISBN."
  (let ((docs '(((title . "A") (isbn . ("1234567890123")))
                 ((title . "B"))
                 ((title . "C") (isbn . ("9876543210987"))))))
    (should (= (length (bib--open-library-candidates docs)) 2))))

(ert-deftest bib-test-open-library-candidates-empty ()
  "Return nil for an empty list."
  (should (null (bib--open-library-candidates nil))))

;;;; bib--crossref-select-doi

(ert-deftest bib-test-crossref-select-doi-selects-first ()
  "Select the first DOI via mocked `completing-read'."
  (let* ((data '((message . ((items . (((title . ("A Paper"))
                                        (DOI . "10.1234/test")
                                        (author . (((family . "Doe")
                                                     (given . "Jane")))))))))))
         (result
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt candidates &rest _)
                       (caar candidates))))
            (bib--crossref-select-doi data))))
    (should (equal result "10.1234/test"))))

(ert-deftest bib-test-crossref-select-doi-nil-when-no-items ()
  "Return nil when items list is empty."
  (let ((data '((message . ((items))))))
    (should (null (bib--crossref-select-doi data)))))

(ert-deftest bib-test-crossref-select-doi-untitled ()
  "Handle items with no title gracefully."
  (let* ((data '((message . ((items . (((DOI . "10.5678/x"))))))))
         (result
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt candidates &rest _)
                       (caar candidates))))
            (bib--crossref-select-doi data))))
    (should (equal result "10.5678/x"))))

;;;; bib-search-crossref (with mocked HTTP)

(ert-deftest bib-test-search-crossref-returns-doi ()
  "Return a DOI from a mocked Crossref response."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "{\"message\": {\"items\": [{\"title\": [\"Test\"], \"DOI\": \"10.9999/mock\", \"author\": [{\"family\": \"Test\", \"given\": \"A\"}]}]}}"))
            ((symbol-function 'completing-read)
             (lambda (_prompt candidates &rest _)
               (caar candidates))))
    (should (equal (bib-search-crossref "test" "author") "10.9999/mock"))))

(ert-deftest bib-test-search-crossref-network-failure ()
  "Signal error when HTTP request returns nil."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _) nil)))
    (should-error (bib-search-crossref "test" "author"))))

;;;; bib-fetch-abstract-from-crossref

(ert-deftest bib-test-fetch-abstract-crossref-found ()
  "Return abstract from a mocked Crossref response."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "{\"message\": {\"abstract\": \"An abstract.\"}}")))
    (should (equal (bib-fetch-abstract-from-crossref "10.1234/x")
                   "An abstract."))))

(ert-deftest bib-test-fetch-abstract-crossref-missing ()
  "Return nil when the abstract field is absent."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "{\"message\": {\"title\": \"No abstract here\"}}")))
    (should (null (bib-fetch-abstract-from-crossref "10.1234/x")))))

(ert-deftest bib-test-fetch-abstract-crossref-network-failure ()
  "Return nil when HTTP request fails."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _) nil)))
    (should (null (bib-fetch-abstract-from-crossref "10.1234/x")))))

;;;; bib-search-isbn (with mocked HTTP)

(ert-deftest bib-test-search-isbn-returns-isbn ()
  "Return an ISBN from a mocked Open Library response."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "{\"docs\": [{\"title\": \"Test Book\", \"isbn\": [\"9781234567890\"], \"author_name\": [\"Author\"]}]}"))
            ((symbol-function 'completing-read)
             (lambda (_prompt candidates &rest _)
               (caar candidates))))
    (should (equal (bib-search-isbn "test") "9781234567890"))))

(ert-deftest bib-test-search-isbn-no-results ()
  "Signal `user-error' when no docs are returned."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _) "{\"docs\": []}")))
    (should-error (bib-search-isbn "nonexistent") :type 'user-error)))

(ert-deftest bib-test-search-isbn-no-isbn-in-results ()
  "Signal `user-error' when results lack ISBNs."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "{\"docs\": [{\"title\": \"No ISBN Book\"}]}")))
    (should-error (bib-search-isbn "test") :type 'user-error)))

(ert-deftest bib-test-search-isbn-network-failure ()
  "Signal error when HTTP request returns nil."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _) nil)))
    (should-error (bib-search-isbn "test"))))

;;;; bib-fetch-abstract-from-google-books

(ert-deftest bib-test-fetch-abstract-google-books-found ()
  "Return description from a mocked Google Books response."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "{\"items\": [{\"volumeInfo\": {\"description\": \"A great book.\"}}]}")))
    (should (equal (bib-fetch-abstract-from-google-books "9781234567890")
                   "A great book."))))

(ert-deftest bib-test-fetch-abstract-google-books-no-items ()
  "Return nil when no items are found."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _) "{\"totalItems\": 0}")))
    (should (null (bib-fetch-abstract-from-google-books "9781234567890")))))

(ert-deftest bib-test-fetch-abstract-google-books-no-description ()
  "Return nil when volumeInfo lacks a description."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "{\"items\": [{\"volumeInfo\": {\"title\": \"No Desc\"}}]}")))
    (should (null (bib-fetch-abstract-from-google-books "9781234567890")))))

;;;; bib-search-imdb (with mocked HTTP)

(ert-deftest bib-test-search-imdb-returns-url ()
  "Return an IMDB URL from a mocked OMDb response."
  (let ((bib-omdb-key "fake-key"))
    (cl-letf (((symbol-function 'bib--http-get)
               (lambda (_url &rest _)
                 "{\"Search\": [{\"Title\": \"Test Movie\", \"Year\": \"2020\", \"imdbID\": \"tt1234567\"}]}"))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (caar candidates))))
      (should (equal (bib-search-imdb "test")
                     "https://www.imdb.com/title/tt1234567")))))

(ert-deftest bib-test-search-imdb-no-results ()
  "Signal `user-error' when no movies are found."
  (let ((bib-omdb-key "fake-key"))
    (cl-letf (((symbol-function 'bib--http-get)
               (lambda (_url &rest _)
                 "{\"Response\": \"False\"}")))
      (should-error (bib-search-imdb "nonexistent") :type 'user-error))))

(ert-deftest bib-test-search-imdb-missing-key ()
  "Signal `user-error' when OMDb key is empty."
  (let ((bib-omdb-key ""))
    (should-error (bib-search-imdb "test") :type 'user-error)))

(ert-deftest bib-test-search-imdb-network-failure ()
  "Signal error when HTTP request returns nil."
  (let ((bib-omdb-key "fake-key"))
    (cl-letf (((symbol-function 'bib--http-get)
               (lambda (_url &rest _) nil)))
      (should-error (bib-search-imdb "test")))))

;;;; bib-translate-title-into-english

(ert-deftest bib-test-translate-title-returns-english ()
  "Return the English title from a mocked TMDb response."
  (let ((bib-tmdb-key "fake-key"))
    (cl-letf (((symbol-function 'bib--http-get)
               (lambda (_url &rest _)
                 "{\"results\": [{\"title\": \"The English Title\"}]}")))
      (should (equal (bib-translate-title-into-english "Le titre")
                     "The English Title")))))

(ert-deftest bib-test-translate-title-no-results ()
  "Signal `user-error' when TMDb finds no results."
  (let ((bib-tmdb-key "fake-key"))
    (cl-letf (((symbol-function 'bib--http-get)
               (lambda (_url &rest _) "{\"results\": []}")))
      (should-error (bib-translate-title-into-english "Unknown")
                    :type 'user-error))))

(ert-deftest bib-test-translate-title-missing-key ()
  "Signal `user-error' when TMDb key is empty."
  (let ((bib-tmdb-key ""))
    (should-error (bib-translate-title-into-english "test")
                  :type 'user-error)))

;;;; bib-lbx--items-from-json

(ert-deftest bib-test-lbx-items-from-json-basic ()
  "Parse Letterboxd JSON items into (DISPLAY . SLUG) pairs."
  (let* ((alist '((items . (((name . "Test Film")
                              (year . 2021)
                              (url . "/film/test-film/"))))))
         (items (bib-lbx--items-from-json alist)))
    (should (equal (length items) 1))
    (should (equal (caar items) "Test Film (2021)"))
    (should (equal (cdar items) "test-film"))))

(ert-deftest bib-test-lbx-items-from-json-no-year ()
  "Omit the year from display when it is nil."
  (let* ((alist '((items . (((name . "Old Film") (url . "/film/old-film/"))))))
         (items (bib-lbx--items-from-json alist)))
    (should (equal (caar items) "Old Film"))))

(ert-deftest bib-test-lbx-items-from-json-empty-items ()
  "Return nil when items list is empty."
  (let ((alist '((items))))
    (should (null (bib-lbx--items-from-json alist)))))

(ert-deftest bib-test-lbx-items-from-json-strips-url ()
  "Strip /film/ prefix and trailing slash from URL to get slug."
  (let* ((alist '((items . (((name . "X") (url . "/film/my-slug/"))))))
         (items (bib-lbx--items-from-json alist)))
    (should (equal (cdar items) "my-slug"))))

;;;; bib-lbx--ddg-items

(ert-deftest bib-test-lbx-ddg-items-extracts-slugs ()
  "Extract film slugs from mocked DuckDuckGo HTML."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "letterboxd.com/film/blade-runner/ more text letterboxd.com/film/arrival/")))
    (let ((items (bib-lbx--ddg-items "test")))
      (should (= (length items) 2))
      (should (equal (cdar items) "blade-runner")))))

(ert-deftest bib-test-lbx-ddg-items-deduplicates ()
  "Remove duplicate slugs from DuckDuckGo results."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _)
               "letterboxd.com/film/dune/ xxx letterboxd.com/film/dune/")))
    (let ((items (bib-lbx--ddg-items "test")))
      (should (= (length items) 1)))))

(ert-deftest bib-test-lbx-ddg-items-nil-on-failure ()
  "Return nil when HTTP request fails."
  (cl-letf (((symbol-function 'bib--http-get)
             (lambda (_url &rest _) nil)))
    (should (null (bib-lbx--ddg-items "test")))))

;;;; bib-lbx--json-items

(ert-deftest bib-test-lbx-json-items-first-endpoint ()
  "Return items from the first Letterboxd endpoint."
  (cl-letf (((symbol-function 'bib-lbx--http-get)
             (lambda (_url &rest _)
               "{\"items\": [{\"name\": \"Film\", \"year\": 2020, \"url\": \"/film/film/\"}]}"))
            ((symbol-function 'bib--parse-json)
             (lambda (text)
               (when text
                 (let ((json-object-type 'alist)
                       (json-array-type 'list)
                       (json-key-type 'symbol))
                   (condition-case nil
                       (json-read-from-string text)
                     (json-error nil)))))))
    (let ((items (bib-lbx--json-items "test")))
      (should (= (length items) 1)))))

(ert-deftest bib-test-lbx-json-items-nil-on-failure ()
  "Return nil when both Letterboxd endpoints fail."
  (cl-letf (((symbol-function 'bib-lbx--http-get)
             (lambda (_url &rest _) nil)))
    (should (null (bib-lbx--json-items "test")))))

;;;; bib-lbx--fetch-items

(ert-deftest bib-test-lbx-fetch-items-prefers-json ()
  "Use JSON items when available."
  (cl-letf (((symbol-function 'bib-lbx--json-items)
             (lambda (_q) '(("Film (2020)" . "film")))))
    (let ((items (bib-lbx--fetch-items "test")))
      (should (= (length items) 1)))))

(ert-deftest bib-test-lbx-fetch-items-falls-back-to-ddg ()
  "Fall back to DuckDuckGo when JSON returns nil."
  (cl-letf (((symbol-function 'bib-lbx--json-items)
             (lambda (_q) nil))
            ((symbol-function 'bib-lbx--ddg-items)
             (lambda (_q) '(("Film" . "film")))))
    (let ((items (bib-lbx--fetch-items "test")))
      (should (= (length items) 1)))))

(ert-deftest bib-test-lbx-fetch-items-error-when-both-fail ()
  "Signal error when both JSON and DuckDuckGo return nil."
  (cl-letf (((symbol-function 'bib-lbx--json-items)
             (lambda (_q) nil))
            ((symbol-function 'bib-lbx--ddg-items)
             (lambda (_q) nil)))
    (should-error (bib-lbx--fetch-items "test"))))

;;;; bib-search-letterboxd (with mocked network)

(ert-deftest bib-test-search-letterboxd-returns-url ()
  "Return a Letterboxd URL by default."
  (let ((bib-letterboxd-use-slug-p nil))
    (cl-letf (((symbol-function 'bib-lbx--fetch-items)
               (lambda (_q) '(("Film (2020)" . "test-film"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (caar candidates))))
      (should (equal (bib-search-letterboxd "test")
                     "https://letterboxd.com/film/test-film/")))))

(ert-deftest bib-test-search-letterboxd-returns-slug ()
  "Return the slug when `bib-letterboxd-use-slug-p' is non-nil."
  (let ((bib-letterboxd-use-slug-p t))
    (cl-letf (((symbol-function 'bib-lbx--fetch-items)
               (lambda (_q) '(("Film (2020)" . "test-film"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (caar candidates))))
      (should (equal (bib-search-letterboxd "test")
                     "test-film")))))

(ert-deftest bib-test-search-letterboxd-full-url-override ()
  "Return the full URL when FULL-URL argument is non-nil."
  (let ((bib-letterboxd-use-slug-p t))
    (cl-letf (((symbol-function 'bib-lbx--fetch-items)
               (lambda (_q) '(("Film (2020)" . "test-film"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (caar candidates))))
      (should (equal (bib-search-letterboxd "test" t)
                     "https://letterboxd.com/film/test-film/")))))

;;;; bib--http-get (with mocked url-retrieve-synchronously)

(ert-deftest bib-test-http-get-returns-body ()
  "Extract the response body after the HTTP header separator."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _)
               (let ((buf (generate-new-buffer " *test-http*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello World"))
                 buf))))
    (should (equal (bib--http-get "http://example.com") "Hello World"))))

(ert-deftest bib-test-http-get-returns-nil-on-failure ()
  "Return nil when `url-retrieve-synchronously' returns nil."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _) nil)))
    (should (null (bib--http-get "http://example.com")))))

(ert-deftest bib-test-http-get-cleans-up-buffer ()
  "Kill the response buffer after extracting body."
  (let (captured-buf)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _)
                 (let ((buf (generate-new-buffer " *test-http-cleanup*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\r\n\r\nBody"))
                   (setq captured-buf buf)
                   buf))))
      (bib--http-get "http://example.com")
      (should-not (buffer-live-p captured-buf)))))

;;;; bib--completing-read

(ert-deftest bib-test-completing-read-returns-value ()
  "Return the VALUE from the selected (DISPLAY . VALUE) pair."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt candidates &rest _)
               (caar candidates))))
    (let ((candidates '(("Display A" . "value-a") ("Display B" . "value-b"))))
      (should (equal (bib--completing-read "Pick: " candidates) "value-a")))))

(provide 'bib-test)

;;; bib-test.el ends here
