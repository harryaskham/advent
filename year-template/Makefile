
pre-init:
	git mv year-template.cabal "x$$(date +%Y).cabal"  # EXCLUDE
	find . -type f | xargs sed -i /EXCLUDE/! s/year-template/x$$(date +%Y)/g"  # EXCLUDE

hpack:
	hpack

gen-hie:
	gen-hie . > hie.yaml

init: pre-init hpack gen-hie
	echo "Init complete."

copy-day-666:
	echo "Copy Day666.hs over other days? (destructive)" && read ans && [ $${ans:-N} = y ];
	for i in $$(seq 1 25); do \
		sed "s/666/$$i/g" src/Day666.hs > src/Day$$i.hs; \
	done

run: hpack gen-hie
	cabal v2-run year-template

repl: hpack gen-hie
	cabal v2-repl

test: hpack gen-hie
	cabal v2-test
