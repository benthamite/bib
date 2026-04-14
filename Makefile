EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch \
	  -L . \
	  -l bib.el \
	  -l bib-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch \
	  -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile bib.el

clean:
	rm -f *.elc
