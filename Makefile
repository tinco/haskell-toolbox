all:
	stack build

run:
	stack exec haskell-toolbox build

deploy:
	rsync -aP _site/ haskell-toolbox.com:/srv/http/haskell-toolbox/

clean:
	stack exec haskell-toolbox clean
