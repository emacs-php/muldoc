EMACS ?= emacs
EASK ?= eask

install:
	$(EASK) package
	$(EASK) install

compile:
	$(EASK) compile

ci: clean autoloads install compile

all: autoloads compile

autoloads:
	$(EASK) generate autoloads

clean:
	$(EASK) clean all

test: clean all
	$(EASK) test ert tests/muldoc-test.el
