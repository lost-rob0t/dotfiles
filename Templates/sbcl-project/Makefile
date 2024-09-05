##
# Project Title
#
# @file
# @version 0.1

##
# Starintel Gserver
#
# @file
# @version 0.1

LISP ?= sbcl

all: test

run:
	$(LISP) --load __PROJECT_NAME__.lisp

build:
	$(LISP)	--non-interactive \
		--load source/starintel-gserver.asd \
		--eval '(ql:quickload :__PROJECT_NAME__)' \
		--eval "(sb-ext:save-lisp-and-die \"__BIN_NAME__\" :toplevel 'star::main :executable t :compression t)"
install:
	cp ./bin/__BIN_NAME__

clean:
	rm -f ./bin/*


# end
