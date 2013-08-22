ghc bench -O2 -threaded
./bench --samples=50 -o bench.html -t report.tpl +RTS -N
