build:
	elm make src/Main.elm --output=out/main.js

test:
	elm-test

test-watch:
	elm-test --watch
