pre-init:
	git mv year-template.cabal "x$$(date +%Y).cabal"  # EXCLUDE
	find . -type f | xargs sed -i "/EXCLUDE/! s/year-template/x$$(date +%Y)/g"  # EXCLUDE

hpack:
	hpack

gen-hie:
	gen-hie . > hie.yaml

init: pre-init hpack gen-hie
	echo "Init complete."

copy-day-666:
	echo "Copy Day666.hs over other days? (destructive)" && read ans && [ $${ans:-N} = y ];
	for i in $$(seq $(FROM) $(TO)); do \
		sed "s/666/$$i/g" src/Day666.hs > src/Day$$i.hs; \
	done

pre-run: hpack gen-hie

run-all: pre-run
	cabal v2-run x2024

repl: pre-run
	cabal v2-repl

test: pre-run
	ghciwatch --test-ghci Spec.main \
	--command "cabal v2-repl test-x2024" \
	--after-startup-ghci '$(AFTER_STARTUP_GHCI)' \
	--watch src \
	--watch input \
	--watch input/examples \
	--watch test \
	--restart-glob '**/package.yaml' \
	--poll 1s

test-today: pre-run
	make AFTER_STARTUP_GHCI=":set args \"--match=/Day $$(date +%-d)/\"" test

run-cmd: pre-run
	ghciwatch --test-ghci $(CMD) \
	--enable-eval \
	--watch src \
	--watch input \
	--watch input/examples \
	--watch lib \
	--restart-glob '**/package.yaml' \
	--restart-glob 'input/**' \
	--poll 1s

run-part:
	make CMD="Day$(DAY).part$(PART)" run-cmd

part1:
	make DAY=$$(date +%-d) PART=1 run-part

part2:
	make DAY=$$(date +%-d) PART=2 run-part

day:
	make CMD="\"print Day$(DAY).part1 >> print Day$(DAY).part2\"" run-cmd

today:
	make DAY="$$(date +%-d)" day
