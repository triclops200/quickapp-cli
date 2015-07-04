ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PACKAGE=quickapp-cli
OUT=quickapp
ENTRY=-main

$(OUT): buildapp quickapp-cli.lisp quicklisp-manifest.txt
	./buildapp  --manifest-file quicklisp-manifest.txt \
				--load-system asdf \
				--eval '(push "$(ROOT_DIR)/" asdf:*central-registry*)' \
				--load-system $(PACKAGE) \
				--eval '($(PACKAGE):disable-debugger)' \
				--output $(OUT) --entry $(PACKAGE):$(ENTRY)

quicklisp-manifest.txt:
	sbcl --non-interactive \
		--eval '(push #P"$(ROOT_DIR)/" asdf:*central-registry*)'\
		--eval '(ql:quickload "$(PACKAGE)")'\
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

buildapp:
	sbcl --eval '(ql:quickload "buildapp")' --eval '(buildapp:build-buildapp)' --non-interactive

clean:
	rm -f *.fasl $(OUT) buildapp quicklisp-manifest.txt
