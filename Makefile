.PHONY: all
all:
	pulp browserify --to output.js
	cp output.js ~/html/dist/ps
