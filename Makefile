all: build run

build:
	elm-multitier-make src/Main.elm

run:
	node server.js
