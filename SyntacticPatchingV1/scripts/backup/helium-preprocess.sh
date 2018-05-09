#!/bin/bash

# preprocess all prjects in folder

for fname in $1/*
do
  echo $fname
  helium --pre $fname > /dev/null 2>&1
  if [ $? == 0 ]; then
    echo "OK"
  else
    echo "FAIL"
  fi
done
