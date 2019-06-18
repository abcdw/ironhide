.PHONY: test

test:
	clojure -A:test:runner

repl:
	clojure -A:test:nrepl
