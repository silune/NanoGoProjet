#!/bin/sh

echo "testing $1 ..."
echo "--------------------"
echo "-    GO OUTPUT     -"
echo "--------------------"
echo ""
go run $1.go
echo ""
echo "--------------------"
echo "-   NGOC OUTPUT    -"
echo "--------------------"
echo ""
./../ngoc $1.go && gcc -no-pie $1.s -o $1.out && ./$1.out
echo ""
rm "$1.s" "$1.out"
