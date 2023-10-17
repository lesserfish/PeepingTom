run:
	cabal install --installdir=./build/ --overwrite-policy=always 
	@echo "\n\n\n"
	time sudo ./build/PeepingTom
check:
	LD_LIBRARY_PATH="/home/lesserfish/Documents/Code/PeepingTom/test/scanmem/build/.libs/:$$LD_LIBRARY_PATH" cabal test
run-test:
	@if [ -z "$(test)" ]; then \
        LD_LIBRARY_PATH="/home/lesserfish/Documents/Code/PeepingTom/test/scanmem/build/.libs/:$$LD_LIBRARY_PATH" cabal run PeepingTom-test; \
    else \
        LD_LIBRARY_PATH="/home/lesserfish/Documents/Code/PeepingTom/test/scanmem/build/.libs/:$$LD_LIBRARY_PATH" cabal run PeepingTom-test -- "$(test)"; \
    fi
