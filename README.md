# Projet Programmation 1 at ENS Paris Saclay

The goal is to achieve a compiler of a smaller version of GoLang called *NanoGo*.

How to use it:
 - `make` to compile the project
 - `./ngoc file.go` to compile a NanoGo code in x86_64
 - `gcc -no-pie file.s -o file` to compile the `.s` file using GCC

## Remarks about the test

There is some scripts to test the `.go` files in `tst` :
 - `all_test.sh` executes all test and waits for a button to be pressed between every test
 - `one_test.sh` executes only one test and takes the name of the file without `.go`
 - `with_go_test.sh` executes only one test but also execute the code with `go run` (also takes the name without `.go`)

Thooses script remove the auxilary files generated.
