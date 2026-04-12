# `bib`: Rudimentary support for bibliographic information retrieval

## Overview

`bib` is a small Emacs package for quickly retrieving bibliographic metadata for books, academic papers, and films. Given only a title (and optionally an author), it searches the relevant public APIs -- Crossref, Open Library, OMDb, TMDb, Letterboxd -- locates the correct unique identifier (DOI, ISBN, IMDb ID, or Letterboxd slug), and returns a ready-to-use result: a DOI string, an IMDb URL, a Letterboxd slug, or a full BibTeX entry (via [zotra](https://github.com/mpedramfar/zotra)).

If you regularly add references to a BibTeX file and want a fast way to go from "I know the title" to "I have the DOI/ISBN/IMDb URL", `bib` handles the lookup-and-select workflow so you don't have to leave Emacs. The interactive commands present a completion list of candidates from the upstream API, and the selected identifier is returned (and, where applicable, copied to the kill ring).

This is not a full-featured reference manager -- just a focused tool for identifier lookup and BibTeX entry generation.

## Installation

`bib` requires Emacs 28.1 or later. All of its runtime dependencies (`url`, `json`, `dom`, `seq`) are built-in. The optional [zotra](https://github.com/mpedramfar/zotra) package is only needed if you want to use `bib-zotra-add-entry-from-title` to generate BibTeX entries.

### package-vc (built-in since Emacs 30)

```emacs-lisp
(use-package bib
  :vc (:url "https://github.com/benthamite/bib"))
```

### Elpaca

```emacs-lisp
(use-package bib
  :ensure (:host github :repo "benthamite/bib"))
```

### straight.el

```emacs-lisp
(use-package bib
  :straight (:host github :repo "benthamite/bib"))
```

## Quick start

Some commands require API keys. `bib-search-crossref` and `bib-search-isbn` work without any key. For the others, set the relevant keys:

```emacs-lisp
(use-package bib
  :ensure (bib :host github :repo "benthamite/bib")
  :custom
  (bib-omdb-key   "your-omdb-key")    ; for bib-search-imdb
  (bib-tmdb-key   "your-tmdb-key"))   ; for bib-translate-title-into-english
```

Then try `M-x bib-search-crossref` or `M-x bib-search-isbn` (no API key needed) -- enter a title, pick from the completion candidates, and the identifier is returned.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).

## License

GPL-3.0 -- see [COPYING.txt](COPYING.txt).
