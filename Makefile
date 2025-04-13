FOURMOLU_EXTENSIONS := -o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms

format:
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence $$(find */src/ */test/ */app/ -iregex ".*.hs")
	cabal-fmt -i $$(find */* -iregex ".*.cabal")

build-echo:
	cabal build echo &&  cabal install echo --installdir ./bins --overwrite-policy=always

run-echo:
	./maelstrom/maelstrom test -w echo --bin ./bins/echo --nodes n1 --time-limit 10 --log-stderr