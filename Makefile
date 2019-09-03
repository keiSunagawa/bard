.PHONY: init
init:
	npm install
	bower install

.PHONY: build
build:
	mkdir -p ~/html/ps
	pulp browserify --to output.js
	cp output.js ~/html/ps
	cp index.html ~/html/ps/
