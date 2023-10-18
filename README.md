# PeepingTom

PeepingTom is a library and tool written in Haskell designed to scan the virtual memory of processes, and extract regions of interest. It behaves similarly to <cite>[ScanMem][1]</cite> or <cite>[Cheat Engine][2]</cite>.

PeepingTom initially reads and parses the virtual memory region information located in /proc/pid/maps. It filters regions of interest, and uses Haskell's FFI to call pread and pwrite on /proc/pid/mem. 

This allow us to directly read and write to a process's virtual memory, letting us farm that *sweet* *sweet* <cite>[Monkey Money][3]</cite> in Bloons TD 6 before we eventually get banned.

## Building

In order to build, ...

## Testing

Automatic tests are provided, in order to ensure that PeepingTom matches the results of ScanMem. In order to run the automatic tests, you need to clone and build ScanMem.

Start by cloning PeepingTom with the *--recurse-submodules* flag enabled.

    git clone --recurse-submodules https://github.com/lesserfish/PeepingTom.git

This will not only clone PeepingTom, but clone scanmem directly on test/scanmem/ directory. We then need to build scanmem. The library files should be located in tests/scanmem/build/.libs. In order to ensure this, follow these steps

    cd test/scanmem/
    ./autogen.sh
    mkdir build && cd build
    ../configure
    make

This will construct the necessary lib files that PeepingTom-test requires to run.
In order to run the tests, run the following command:
    
    LD_LIBRARY_PATH="$PWD/test/scanmem/build/.libs/:$LD_LIBRARY_PATH" cabal test
or
    
    LD_LIBRARY_PATH="$PWD/test/scanmem/build/.libs/:$LD_LIBRARY_PATH" cabal run PeepingTom-test
 
## Library Usage

## Application Usage


[1]: https://github.com/scanmem/scanmem
[2]: https://www.cheatengine.org/
[3]: https://bloons.fandom.com/wiki/Monkey_Money
