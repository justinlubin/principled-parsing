build:
	elm make src/Main.elm --output=docs/main.js

test:
	elm-test

test-watch:
	elm-test --watch
