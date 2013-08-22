@echo off
ghc bench -O2 -threaded
bench.exe --samples=20 -o bench.html -t report.tpl +RTS -N
