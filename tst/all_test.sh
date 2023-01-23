#!/bin/bash
good_test=$(find good/*.go)
bad_typing=$(find bad_typing/*.go)
bad_compile=$(find bad_compile/*.go)

echo ""
echo "--------------------"
echo "-    GOOD TEST     -"
echo "--------------------"
echo ""
for f in $good_test; do
  NAME=$(echo $f | cut -d '.' -f 1)
  echo "Testing $f :"
  ./../ngoc $NAME.go && gcc -no-pie $NAME.s -o $NAME.out && ./$NAME.out
 read -n 1 -s
done  

echo ""
echo "--------------------"
echo "-       DONE       -"
echo "--------------------"
echo "--------------------"
echo "-    BAD TYPING    -"
echo "--------------------"
echo ""
for f in $bad_typing; do
  NAME=$(echo $f | cut -d '.' -f 1)
  echo "Testing $f :"
  ./../ngoc $NAME.go && gcc -no-pie $NAME.s -o $NAME.out && ./$NAME.out
  read -n 1 -s
done
echo ""
echo "--------------------"
echo "-       DONE       -"
echo "--------------------"
echo ""
echo "--------------------"
echo "-    BAD COMPILE   -"
echo "--------------------"
echo ""
for f in $bad_compile; do
  NAME=$(echo $f | cut -d '.' -f 1)
  echo "Testing $f :"
  ./../ngoc $NAME.go && gcc -no-pie $NAME.s -o $NAME.out && ./$NAME.out
  read -n 1 -s
done
echo ""
echo "--------------------"
echo "-       DONE       -"
echo "--------------------"
echo ""




rm good/*.s good/*.out bad_typing/*.s bad_typing/*.out bad_compile/*.s bad_compile/*.out
