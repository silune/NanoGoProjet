#!/bin/bash
good_test=$(find good/*.go)
bad_test=$(find bad/*.go)

echo "---------- GOOD TEST ---------"
echo ""
for f in $good_test; do
  echo "Testing $f :"
  ./../ngoc "$f"
  sleep 1
done  

echo "---------- DONE ---------------"
echo ""
echo "---------- BAD TEST -----------"
echo ""
for f in $bad_test; do
  echo "Testing $f :"
  ./../ngoc "$f"
  sleep 1
done

echo "---------- DONE ---------------"
echo ""

rest_good=$(find good/*.s)
rest_bad=$(find bad/*.s)

for f in $rest_good; do
  rm "$f"
done

for f in $rest_bad; do
  rm "$f"
done
