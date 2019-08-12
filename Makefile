EMACS ?= emacs
ELS = mldoc.el
AUTOLOADS = mldoc-autoloads.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

all: autoloads $(ELCS)

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (package-generate-autoloads \"mldoc\" default-directory))"

clean:
	rm -f $(ELCS) $(AUTOLOADS)

test: clean all
	$(EMACS) -Q -batch -L . -l mldoc-test.el -f ert-run-tests-batch-and-exit
