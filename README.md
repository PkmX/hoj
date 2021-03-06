# Installation

Clone the repository recursively:
```
$ git clone --recursive git@github.com:PkmX/hoj.git
```

Or update all submodules manually after cloning:
```
$ git submodule update
```

## Client

```
$ pushd client && npm install && npm run build && popd
```

## Server

```
$ pushd server && cabal sandbox init && cabal install && popd
```

## EasySandbox

```
$ pushd EasySandbox && make all runtests && popd
$ ln -s EasySandbox/EasySandbox.so libEasySandbox.so
```

# Running

```
$ server/.cabal-sandbox/bin/hoj --help
Haskell Online Judge System

Usage: hoj [--bind ip-address] [--port port] [--debug] [--timelimit μs]
           [--clientdir directory] [--problempdf problem.pdf]
           [[test-input test-output]...]

Available options:
  -h,--help                Show this help text
  --bind ip-address        The address to listen on
  --port port              The port to listen on
  --debug                  Enable debugging
  --timelimit μs           Time limit in microseconds
  --clientdir directory    Client directory to serve
  --problempdf problem.pdf The problem PDF to serve
```

Example:

```
$ server/.cabal-sandbox/bin/hoj --port 12345 --problempdf quiz/problem.pdf quiz/1.in quiz/1.out quiz/2.in quiz/2.out
```
