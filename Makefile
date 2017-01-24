all: build run

build:
	elm-multitier-make src/chat/Main.elm

run:
	node server.js
