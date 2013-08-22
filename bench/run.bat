@echo off
ghc bench -O2 -threaded
bench.exe --samples=50 -o bench.html -t report.tpl +RTS -N
