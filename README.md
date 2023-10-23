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

### Initial Scan

Once you launch PeepingTom, you will be faced with a prompt.

Typing help will give you a list of available commands

    > help

    PeepingTom: A Virtual memory scanner.
    
    The following commands are available: 
    	 pid:                         Sets the PID of the process to be scanned by PeepingTom. 
    	 $ [filter] [value]:          Scans the memory or update the candidates, and extracts those that satisfy the filter. 
    	 set [option] [args]:         Sets various options regarding the scan. Type 'help set' to see more.
    	 list [object]:               Lists several objects. To see a list of available objects type 'help list'.
    	 delete [object]:             Deletes an object. To see a list of objects that can be deleted type 'help delete'.
    	 filter map [filter]:         Filters the list of virtual memory regions. To see a list of available filters type 'list rfilter'.
    	 filter candidate [filter]:   Filters the list of candidates. To see a list of available filters type 'list filter'.
    	 update:                      Update the values of the current candidates.
    	 save [name]:                 Save the current list of candidates.
    	 load [name]:                 Loads a list of candidates.
    	 new [name]:                  Creates a new list of candidates.
    	 reset:                       Resets the current list of candidates.

You can set the PID of the process you want to scan by running

    > pid 12345678

Once you set the PID, you can perform an initial scan.

All scans require a filter, a list of available filters can be seen by using

    > help $

For example, in order to extract all memory addresses that contain the integer value 27, you would

    > $ == 27

The initial scan can take some time, depending on the quantity of regions of interest.

The scan will send a SIGSTOP before starting, and a SIGCONT when it ends. This can be deactivated by running

    > set send_stopsig false

### Future scans

Future scans can be done similarly to initial scans, by simple running

    > $ == 23

Future scans will usually be a lot faster, depending on how many candidates were extracted in the initial scan.

To list the available candidates you can run:

    > list state

This will show you up to 5 regions and up to 5 candidates. You can specify the number of regions / candidates by adding arguments to 'list state'

    > list state 10 2

This will list 10 candidates and 2 regions. By passing -1 as an argument, you can print all of the candidates. 

You can update the value of the candidates by running

    > update

This will read the virtual memory, and update the values of each candidates. It will then print the results.

### Setting values

Once you have filtered the virtual memory, and isolated your addresses of interest, you may want to set the addresses to a current value.

Currently, you can only set the memory value to an integer value. You can do this by running

    > IntSet value

This will iterate over all of the candidates, and set the memory value to the corresponding value, taking into consideration the length of the memory address, i.e. It will consider whether the address holds an Int8 or an Int32. 

### Multiple searches

You can save a scan by running

    > save name

You load the saved scan by running

    > load name

Different sessions can scan different processes. For example,

    > pid 12345678
    > $ == 42
    > save age_of_empires
    > new civ5
    civ5 > pid 87654321
    civ5 > $ == 41

This will search the two different processes. All future searches will be done using the PID used in the initial scan, so you don't have to repeatedly set pid every time you want to refine your search.

You can reset the current session by running reset

    civ5 > reset

The following scann will then be equivalent to a initial scan. To see a list of saved scans, you can run

    > show memory

### Options

You can set the type of interest by running

    > set type int64 int16 int 8

So far, only Int64, Int32, Int16 and Int8 are supported.

PeepingTom scans the regions of virtual memory by reading chunks of memory, and then scanning each chunk before loading the next chunk. You can set the size of chunks being loaded by running

    > set chunk_size 1000

You can also print the current options by running

    > list options



[1]: https://github.com/scanmem/scanmem
[2]: https://www.cheatengine.org/
[3]: https://bloons.fandom.com/wiki/Monkey_Money
