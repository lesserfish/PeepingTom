run:
	cabal install --installdir=./build/ --overwrite-policy=always 
	@echo "\n\n\n"
	time sudo ./build/PeepingTom
