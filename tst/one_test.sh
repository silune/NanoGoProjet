#!/bin/sh

echo "testing $1 ..."
./../ngoc $1.go && gcc -no-pie $1.s -o $1.out && ./$1.out

rm "$1.s" "$1.out"
