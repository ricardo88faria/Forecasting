.PHONY: run tail kill

run:
	./forecast.R > log.out

tail:
	tail -f log.out

kill:
	killall R
