all: build

build:
	stack exec haskell-toolbox-main build

docker:
	docker build -t tinco/haskell-toolbox .
	docker push tinco/haskell-toolbox

.PHONY: all test clean docker
