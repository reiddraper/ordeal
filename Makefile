.PHONY: all compile lint

all: compile lint

compile:
	@stack build

lint:
	@hlint app src
