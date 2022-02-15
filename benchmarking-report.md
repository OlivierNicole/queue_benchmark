# Benchmarking four implementations of the same concurrent queue – Discussion

## Benchmark code

I chose the benchmark to minimize contention. It consists in two OCaml domains,
one producer and one consumer, mutating a shared queue. The producer enqueues
all integers from 1 to N, while the consumer dequeues and sums them.
I use a queue of capacity 1024, as my experiments suggest that with this
parameter, no thread spends a significant amount of time blocked due to the
queue being full (resp. empty).

Note that each enqueuing and dequeuing allocates a few words (around 6), I
haven't looked deeply into it but I think it is due to code performing the
exponential backoff. Since N = 120,000,000, in total many words are allocated.
Around 10,000 minor collections and 500 major collections are performed in a
run, for all implementations.

## Implementations of the queue

There are four implementations. The implementation logic is always the same,
only the layout of data varies. The implemented queue is the one described in
<https://dl.acm.org/doi/pdf/10.1145/3473571>. One notable point is that it
tries to limit competition between enqueuers and dequeuers: when the queue is
nor empty nor full, enqueuers and dequeuers operate on separate references and
do not compete.

- **original:** The queue is an array of cells; a cell is a record consisting of
  an element of type `'a`, and a timestamp of type `int Atomic.t`.
  In addition to the array, the queue stores a head and a tail in the form of
  two `int Atomic.t`s.
- **array of atomics:** The queue is reshaped in two arrays, an `'a array` for
  the queued elements, and an `int Atomic.t array` for the timestamps. This
  is not intended to be faster, but rather as a control benchmark. The `head`
  and `tail` are not modified.
- **flat array:** The queue consists in two arrays, an `'a array` for the queued
  elements, and an `int array` for the timestamps. Atomic accesses in this array
  are performed using the new atomic primitives.
- **flat padded array:** In all previous implementations, the lengths of the
  arrays are equal to the capacity of the queue. This implementation is an
  attempt to minimize cache invalidation, i.e. the cost of maintaining cache
  consistency when two cores perform modifications in the same cache line.
  To do that, the values in both arrays are spaced by a padding of 7 words,
  making sure that each sits on its own cache line (on my processor, cache lines
  are 64 bytes long). Dummy values are used as padding.

## Results

Original implementation:
```
major collections: 513, minor collections: 10052, minor allocated: 1464351153.000000


 Performance counter stats for './_build/default/bench.exe -orig':

         38 446,67 msec task-clock:u              #    1,999 CPUs utilized          
                 0      context-switches:u        #    0,000 K/sec                  
                 0      cpu-migrations:u          #    0,000 K/sec                  
             1 657      page-faults:u             #    0,043 K/sec                  
   107 145 629 228      cycles:u                  #    2,787 GHz                    
    46 443 167 858      instructions:u            #    0,43  insn per cycle         
     9 430 403 685      branches:u                #  245,285 M/sec                  
         8 131 444      branch-misses:u           #    0,09% of all branches        
         2 660 276      cache-misses:u                                              

      19,230087913 seconds time elapsed

      38,544138000 seconds user
       0,079319000 seconds sys
```

Array of atomics:
```
major collections: 442, minor collections: 10160, minor allocated: 1493390635.000000


 Performance counter stats for './_build/default/bench.exe -arratom':

         39 290,20 msec task-clock:u              #    2,000 CPUs utilized          
                 0      context-switches:u        #    0,000 K/sec                  
                 0      cpu-migrations:u          #    0,000 K/sec                  
             1 658      page-faults:u             #    0,042 K/sec                  
   109 474 502 690      cycles:u                  #    2,786 GHz                    
    51 071 256 990      instructions:u            #    0,47  insn per cycle         
    10 796 414 923      branches:u                #  274,786 M/sec                  
        14 530 529      branch-misses:u           #    0,13% of all branches        
        11 253 536      cache-misses:u                                              

      19,643553219 seconds time elapsed

      39,365280000 seconds user
       0,070193000 seconds sys
```

Flat array:
```
major collections: 626, minor collections: 11007, minor allocated: 1518965213.000000


 Performance counter stats for './_build/default/bench.exe -atomarr':

         44 899,90 msec task-clock:u              #    2,000 CPUs utilized          
                 0      context-switches:u        #    0,000 K/sec                  
                 0      cpu-migrations:u          #    0,000 K/sec                  
             1 644      page-faults:u             #    0,037 K/sec                  
   125 124 480 415      cycles:u                  #    2,787 GHz                    
    65 590 029 260      instructions:u            #    0,52  insn per cycle         
    12 564 061 946      branches:u                #  279,824 M/sec                  
        15 708 293      branch-misses:u           #    0,13% of all branches        
         5 050 209      cache-misses:u                                              

      22,451070497 seconds time elapsed

      44,989052000 seconds user
       0,105370000 seconds sys
```

Padded flat array:
```
major collections: 318, minor collections: 9401, minor allocated: 1457538695.000000


 Performance counter stats for './_build/default/bench.exe -atomarropt':

         36 364,28 msec task-clock:u              #    2,001 CPUs utilized          
                 0      context-switches:u        #    0,000 K/sec                  
                 0      cpu-migrations:u          #    0,000 K/sec                  
             1 685      page-faults:u             #    0,046 K/sec                  
   101 339 056 458      cycles:u                  #    2,787 GHz                    
    64 994 758 938      instructions:u            #    0,64  insn per cycle         
    12 372 917 041      branches:u                #  340,249 M/sec                  
        38 979 283      branch-misses:u           #    0,32% of all branches        
         9 979 203      cache-misses:u                                              

      18,173489965 seconds time elapsed

      36,399066000 seconds user
       0,065152000 seconds sys
```

## Unordered ideas

Maybe I should try something other than integers.
