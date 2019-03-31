# sssp

Implements Dijkstra and Bellman-Ford algorithms to solve single-source shortest paths (SSSP) problem.

## Setup

1. Get `stack` from [here](https://docs.haskellstack.org/en/stable/README/)
2. Download this project as a zip and extract it to a directory
3. `cd` to that directory in a terminal
4. Run `stack setup`

## Building

```
stack build
```

## Running

Running the executable will benchmark each SSSP algorithm (Dijkstra and Bellman Ford).

To run the benchmarks and output the results to `results.html`:
```
stack exec -- sssp-exe --output results.html
```

## Testing

To run the unit tests:
```
stack test
```
