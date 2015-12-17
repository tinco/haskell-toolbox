all:
	stack build

run:
	stack exec haskell-toolbox build

clean:
	stack exec haskell-toolbox clean
