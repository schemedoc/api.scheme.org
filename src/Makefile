.PHONY : all test

all :

test : test-repl test-metadata test-render test-response

FORCE :
MD_PATH         ?= /home/frank/localsrc/implementation-metadata
DEBUG_OFF       ?= '--debug=\#f'
DEBUG_ALL       ?= '--debug=\#t'
.PHONY : run-http-server

run-http-server :
	MD_PATH=$(MD_PATH) guile  -L "$(CURDIR)/guile" -L "$(CURDIR)/common" -c "(use-modules (sdp server http)) (http-server)"

run-test-http-server :
	MD_PATH=$(MD_PATH) guile  -L "$(CURDIR)/guile" -L "$(CURDIR)/common" -c "(use-modules (sdp server http)) (test-http-server)"
	echo "TODO:next: no use run-client-guile to perform some tests on server, then shutdown server"
.PHONY : run-client-guile

run-client-guile :
	MD_PATH=$(MD_PATH) guile  -L "$(CURDIR)/guile" -L "$(CURDIR)/common" -c "(use-modules (sdp client client)) (main)"
.PHONY : test-repl test-repl-gauche test-repl-guile

test-repl : test-repl-gauche test-repl-guile

test-repl-gauche :
	echo "Not yet implemented: " $@
	MD_PATH=$(MD_PATH) gosh -A"$(CURDIR)/gauche" -A"$(CURDIR)/common/sdp/common" -e"(use sdp.common.repl-gauche)" -e"(test)" -e"(exit)" -- $(DEBUG_ALL)
	tail -n2 test-repl-gauche.log

test-repl-guile :
	MD_PATH=$(MD_PATH) guile  -L "$(CURDIR)/guile" -L "$(CURDIR)/common" -c "(use-modules (sdp common repl-guile)) (test)" $(DEBUG_ALL)
	tail -n2 test-repl-guile.log
.PHONY : test-metadata test-metadata-gauche test-metadata-guile

test-metadata : test-metadata-gauche test-metadata-guile

test-metadata-gauche :
	MD_PATH=$(MD_PATH) gosh -A"$(CURDIR)/gauche" -A"$(CURDIR)/common/sdp/common" -e"(use sdp.common.metadata-gauche)" -e"(test)" -e"(exit)" -- $(DEBUG_ALL)
	tail -n2 test-metadata-gauche.log

test-metadata-guile :
	MD_PATH=$(MD_PATH) guile  -L "$(CURDIR)/guile" -L "$(CURDIR)/common" -c "(use-modules (sdp common metadata-guile)) (test)" $(DEBUG_ALL)
	tail -n2 test-metadata-guile.log
.PHONY : test-render test-response

test-render :
	MD_PATH=$(MD_PATH) guile  -L "$(CURDIR)/guile" -L "$(CURDIR)/common" -c "(use-modules (sdp server http)) (test-render)" $(DEBUG_ALL)
	tail -n2 test-render-guile.log

test-response :
	MD_PATH=$(MD_PATH) guile  -L "$(CURDIR)/guile" -L "$(CURDIR)/common" -c "(use-modules (sdp server http)) (test-response)" $(DEBUG_ALL)
	tail -n2 test-response-guile.log
