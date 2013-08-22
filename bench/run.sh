ghc bench -O2 -threaded
./bench --samples=20 -o bench.html -t report.tpl +RTS -N
