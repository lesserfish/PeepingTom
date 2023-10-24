


# PeepingTom

PeepingTom is a library and tool written in Haskell designed to scan the virtual memory of processes, and extract regions of interest. It behaves similarly to <cite>[ScanMem][1]</cite> or <cite>[Cheat Engine][2]</cite>.

PeepingTom initially reads and parses the virtual memory region information located in /proc/pid/maps. It filters regions of interest, and uses Haskell's FFI to call pread and pwrite on /proc/pid/mem. 

This allow us to directly read and write to a process's virtual memory, letting us farm that *sweet* *sweet* <cite>[Monkey Money][3]</cite> in Bloons TD 6 before we eventually get banned.

## Preview

The follow is a preview, using the game <cite>[Hotline Miami][4]</cite>




https://github.com/lesserfish/PeepingTom/assets/73536889/4aa90a6a-3cec-42c6-a623-1b162808c3d7




## Building

Building PeepingTom should be relatively straight-forward.

The .cabal files requires cabal version 3.0, and base version >= 4.18.0.0, however, it is likely to compile under previous versions.

If you need to compile this with other versions of cabal, just update the .cabal file.

To compile, simply run

    cabal build

The application will be compiled and saved to dist-newstyle/.../.../somewhere

Since the application relies on having sudo permissions for reading/writing the virtual memory of other processes, you may prefer to run

    mkdir build
    cabal install --installdir=./build/ --overwrite-policy=always 
    sudo ./build/PeepingTom

If you are interested in running the automatic tests, additional steps need to be taken. Please refer to the following section.

## Testing

Automatic tests are provided, in order to ensure that PeepingTom matches the results of ScanMem. In order to run the automatic tests, you need to clone and build ScanMem.

Start by cloning PeepingTom with the *--recurse-submodules* flag enabled.

    git clone --recurse-submodules https://github.com/lesserfish/PeepingTom.git


This will not only clone PeepingTom, but clone scanmem directly on test/scanmem/ directory. We then need to build scanmem. 

Although PeepingTom has no additional requirements, scanmem does. So, in order to support testing, you need to install the following packages:

    sudo apt install autotools-dev libtool libreadline-dev intltool python3

The library files should be located in tests/scanmem/build/.libs. In order to ensure this, follow these steps

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

Future scans can be done similarly, by simply running

    > $ == 23

Future scans will usually be a lot faster, depending on how many candidates were extracted in the initial scan.

To list the available candidates you can run:

    > list state

This will show you up to 5 regions and up to 5 candidates. You can specify the number of candidates / regions by adding arguments to 'list state'

    > list state 10 2

This will list 10 candidates and 2 regions. By passing -1 as an argument, you can print all of the candidates. 

You can update the value of the candidates by running

    > update

This will read the virtual memory, and update the values of each candidates. It will then print the results.

### Setting values

Once you have filtered the virtual memory, and isolated your addresses of interest, you may want to set the addresses to a specific value.

Currently, you can only set the memory value to an integer value. You can do this by running

    > IntSet value

This will iterate over all of the candidates, and set the memory value to the corresponding value, taking into consideration the length of the memory address, i.e. It will consider whether the address holds an Int8 or an Int32. 

### Multiple searches

You can save a scan by running

    > save name

You load the saved scan by running

    > load name

You can create a new scan by running

    > new name
    
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

The following scan will then be equivalent to a initial scan. To see a list of saved scans, you can run

    > show memory

### Options

You can set the type of interest by running

    > set type int64 int16 int 8

So far, only Int64, Int32, Int16 and Int8 are supported.

PeepingTom scans the regions of virtual memory by reading chunks of memory, and then filtering the addresses in each chunk before loading the next chunk. You can set the size of chunks being loaded by running

    > set chunk_size 1000

You can also print the current options by running

    > list options

## Library Usage

PeepingTom is initially an application, and not a library. However, the main methods used by the application have been separated in the form of a library.

This is NOT a public library. I do not have any plans to provide continuous support for this library. All methods can and will change without warning.

PeepingTom is JUST a cool learning project I wrote. It is released under the MIT license, so the following applies:

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

Having said that, if you want to use the library for your own application, the following is an example of how it works. 

### Scanning for virtual memory regions.

In order to identify the regions of virtual memory, you can run the following functions

    import qualified PeepingTom.Maps as Maps

    let pid = 12345678
    map_all <- Maps.getMapInfo pid

This will extract ALL of the regions of virtual memory. We are usually not interested in all of them. In order to filter the regions of virtual memory that are interesting, you can use the default filter:

    let fltr = defaultFilter map_all
    let map = filterMap fltr map_all

This will extract the regions of virtual memory that

1. Have length greather than 0
2. Have read/write permissions set
3. Are not mappings to external files, with the exception of the main executable (which is symlinked in /proc/pid/exe)

The default filter matches the default filter of scanmem.

### Initial scan:

We are interested in extracting the addresses in virtual memory that satisfy a specific filter.

The filters need to be of type: 
    
    type Filter = BS.ByteString -> Type -> Bool

where Type is a custom data type defined in PeepingTom.Type. It is defined as:

    data Type
        = Void
        | Int8
        | Int16
        | Int32
        | Int64
        | UInt8
        | UInt16
        | UInt32
        | UInt64
        | Flt
        | Dbl
        | Bytes Int
        deriving (Show)

So far, there is only a default filter for Integer comparison / equality.

You can construct this filter by running

    import qualified PeepingTom.Filters as Filters
    let fltr_eq = Filters.eqInt 47 -- This will create a filter that selects all regions of memory that contain the value 47 as Int8, Int16, Int32 or Int64
    let fltr_geq = Filters.compareInt (>= 32) -- This will create a filter that selects all reigons of memory that contain a value greather than 32 as Int8, Int16, Int32 or Int64.

Prefer 'Filters.eqInt x' over 'Filters.compareInt (== x)'. It is faster, since it compares ByteStrings instead of relying on a cast.

Once you have a filter, you can perform an initial scan by running scanMap

    scanMap :: [Type] -> Filters.Filter -> Maps.MapInfo -> IO PeepState

To use scanMap, you need to specify a list of possible types, a filter and the map info obtained by 'getMapInfo'.

The following is an example:

    import qualified PeepingTom.Type as T
    import qualified PeepingTom.State as State
    import qualified PeepingTom.Filters as Filters
    import qualified PeepingTom.Maps as Maps

    let pid = 12345678
    map_all <- Maps.getMapInfo pid
    let rfltr = defaultFilter map_all
    let map = filterMap rfltr map_all
    
    let cfltr = Filters.eqInt 42
    ptstate <- State.scanMap [T.Int32, T.Int64] cfltr map

The result is an object PeepState, defined as:

    data PeepState = PeepState
    { psPID :: PID
    , psCandidates :: [Candidate]
    , psRegions :: Maps.MapInfo
    }

### Refining the search

If you are interested in refining your search, you need to

1. Update the peep state
2. Refilter the results

You can do this by running:

    state' <- updateState state

Once you have updated your values, you can simply re-run your filter

    let fltr = Filters.eqInt 33
    let fstate = applyFilter fltr state'

### Setting values

Once you have refined your search enough, you may be interested in setting the memory addresses to a specific value. Since PeepState can hold addresses of various types, each with a different type length, you will need a writer. A writer is defined as

    type Writer = Type -> BS.ByteString

There is an implementation for a signed integer writer in PeepingTom.Writer

The following is an example of its usage:

    import qualified PeepingTom.Writer as Writer
    
    let writer = Writer.writeInt 127
    State.applyWriter writer (State.defaultScanOptions) fstate

This will set each address in fstate to be equal to 127, taking into consideration the size of the data being stored in each address. 

[1]: https://github.com/scanmem/scanmem
[2]: https://www.cheatengine.org/
[3]: https://bloons.fandom.com/wiki/Monkey_Money
[4]: https://store.steampowered.com/app/219150/Hotline_Miami/
