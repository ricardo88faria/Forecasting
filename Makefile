.PHONY: run tail

run:
	./forecast.R >& log

tail:
	tail -f log
