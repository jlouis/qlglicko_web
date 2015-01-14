SRC=main.js d3_graphs.js

.PHONY: all
all: build

.PHONY: build
build: ${SRC}
	browserify ${SRC} -o ../www/static/bundle.js
