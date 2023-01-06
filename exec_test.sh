#!/bin/sh

echo "testing $1 ..."
./ngoc "$1.go" && gcc -no-pie "$1.s" -o "$1" && ./$1
