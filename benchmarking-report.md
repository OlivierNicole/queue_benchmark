# Benchmarking four implementations of the same concurrent queue – Discussion

## Compiler

The compiler used must have atomic array support:
<https://github.com/OlivierNicole/ocaml/tree/5.00%2Batomic_arrays>.

## Benchmark code

The four queue implementation are in four modules at
[queue.ml, ll.
129-512](https://github.com/OlivierNicole/queue_benchmark/blob/master/queue.ml#L129-L512).
The benchmarking code is in
[bench.ml](https://github.com/OlivierNicole/queue_benchmark/blob/master/bench.ml).

I chose the benchmark to minimize contention. It consists in two OCaml domains,
one producer and one consumer, mutating a shared queue. The producer enqueues
all integers from 1 to N, while the consumer dequeues and sums them.
I use a queue of capacity 1024, as my experiments suggest that with this
parameter, no thread spends a significant amount of time blocked due to the
queue being full (resp. empty).

Note that each enqueuing and dequeuing allocates a few words. Since N =
120,000,000, in total many words are allocated.  Around 20,000 minor collections
and a few hundred major collections are performed in a run. However, profiling
shows that GC only accounts for less than 1.5 % of CPU cycles in all
implementations.

## Implementations of the queue

There are four implementations. The implementation logic is always the same,
only the layout of data varies. The implemented queue is the one described in
<https://dl.acm.org/doi/pdf/10.1145/3473571>. One notable point is that it
tries to limit competition between enqueuers and dequeuers: when the queue is
nor empty nor full, enqueuers and dequeuers operate on separate references and
do not compete.
Profiling shows that with our chosen queue capacity of 1024, competition between
the enqueuer and the dequeuer is very low.

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
- **padded flat array:** In all previous implementations, the lengths of the
  arrays are equal to the capacity of the queue. This implementation is an
  attempt to minimize cache invalidation, i.e. the cost of maintaining cache
  consistency when two cores perform modifications in the same cache line.
  To do that, the values in both arrays are spaced by a padding of 7 words,
  making sure that each sits on its own cache line (on my processor, cache lines
  are 64 bytes long). Dummy values are used as padding.

## Results

`instructions` means retired instructions.

Original implementation:
```
major collections: 683, minor collections: 18797, minor allocated: 2670050549.000000


 Performance counter stats for './_build/default/bench.exe -orig':

         36 311,26 msec task-clock                #    2,002 CPUs utilized
             8 427      context-switches          #    0,232 K/sec
               231      cpu-migrations            #    0,006 K/sec
             1 666      page-faults               #    0,046 K/sec
   101 398 595 970      cycles                    #    2,792 GHz
    50 481 945 948      instructions              #    0,50  insn per cycle
    10 003 680 544      branches                  #  275,498 M/sec
        75 043 139      branch-misses             #    0,75% of all branches
         9 419 499      cache-misses

      18,138872914 seconds time elapsed

      36,381363000 seconds user
       0,143656000 seconds sys
```

Array of atomics:
```

major collections: 784, minor collections: 19537, minor allocated: 2712617931.000000


 Performance counter stats for './_build/default/bench.exe -arratom':

         36 556,33 msec task-clock                #    2,002 CPUs utilized
             8 867      context-switches          #    0,243 K/sec
               242      cpu-migrations            #    0,007 K/sec
             1 662      page-faults               #    0,045 K/sec
   102 081 651 365      cycles                    #    2,792 GHz
    55 277 502 548      instructions              #    0,54  insn per cycle
    11 405 761 724      branches                  #  312,005 M/sec
        81 378 525      branch-misses             #    0,71% of all branches
         9 711 078      cache-misses

      18,261561298 seconds time elapsed

      36,642423000 seconds user
       0,145649000 seconds sys
```

Flat array:
```
major collections: 878, minor collections: 19826, minor allocated: 2727334369.000000


 Performance counter stats for './_build/default/bench.exe -atomarr':

         37 412,19 msec task-clock                #    2,002 CPUs utilized
             8 924      context-switches          #    0,239 K/sec
               245      cpu-migrations            #    0,007 K/sec
             1 660      page-faults               #    0,044 K/sec
   104 471 607 617      cycles                    #    2,792 GHz
    69 572 868 476      instructions              #    0,67  insn per cycle
    13 123 650 208      branches                  #  350,785 M/sec
        25 799 769      branch-misses             #    0,20% of all branches
         9 509 333      cache-misses

      18,688023384 seconds time elapsed

      37,481333000 seconds user
       0,178884000 seconds sys
```

Padded flat array:
```
major collections: 784, minor collections: 19223, minor allocated: 2693143039.000000


 Performance counter stats for './_build/default/bench.exe -atomarropt':

         32 894,63 msec task-clock                #    2,002 CPUs utilized
             8 818      context-switches          #    0,268 K/sec
               258      cpu-migrations            #    0,008 K/sec
             1 689      page-faults               #    0,051 K/sec
    91 851 678 812      cycles                    #    2,792 GHz
    69 584 989 979      instructions              #    0,76  insn per cycle
    13 081 472 514      branches                  #  397,678 M/sec
        15 337 510      branch-misses             #    0,12% of all branches
        12 244 895      cache-misses

      16,431515337 seconds time elapsed

      32,990947000 seconds user
       0,140776000 seconds sys
```

### Discussion

- **padded flat array** wins in terms of run time but not by much, given that
  variance between runs is around 10-15 %. This is a little disappointing as I
  expected the removal of one level of indirection to improve the stats a lot
  more.
- I'm not sure how to interpret the higher number of `instructions` for **padded
  flat array**, nor the higher number of cache misses.
- Removing the generative functor `DoBechamelBench` (defined at
  <https://github.com/OlivierNicole/queue_benchmark/blob/master/bench.ml#L105>)
  makes **original** run 3 times faster, although this functor is never
  instantiated. I suspect a micro-architectural optimization conditioned to
  alignment. I don't knwon why only **original** benefits from it.
- This benchmark only considers arrays of integers (or of `int Atomic.t`s).
  Maybe working with more complex values would increase the advantage of flat
  arrays.
