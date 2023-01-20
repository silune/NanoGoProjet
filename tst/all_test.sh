#!/bin/bash
good_test=$(find good/*.go)
bad_test=$(find bad_typing/*.go)

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
echo "-     BAD TEST     -"
echo "--------------------"
echo ""
for f in $bad_test; do
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

rm good/*.s good/*.out bad_typing/*.out bad_typing/*.out
