pre-init:
	#mv year-template.cabal "x$$(date +%Y).cabal"  # EXCLUDE
	find . -type f | xargs sed -i "/EXCLUDE/! s/year-template/x$$(date +%Y)/g"  # EXCLUDE

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

pre-run: hpack gen-hie

run: pre-run
	cabal v2-run x2025

repl: pre-run
	cabal v2-repl

test: pre-run
	cabal v2-test

testwatch: pre-run
	ghciwatch --test-ghci Spec.main \
	--command "cabal -O0 -j --ghc-options=-j v2-repl --enable-multi-repl test-x2025"
	--watch src \
	--watch input \
	--watch test \
	--restart-glob '**/package.yaml' \
	--poll 1s
