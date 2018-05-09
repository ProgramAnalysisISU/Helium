#!/bin/bash
field1="project name"
field2="exit status"
field3="compile success count"
field4="compile error count"
field5="build rate"
field6="run success count"
field7="run error count"
field8="run rate"
echo $field1,$field2,$field3,$field4,$field5
for fname in $1/*
do
  echo -n $fname,
  if [ `uname` == "Darwin" ]; then
    gtimeout 60 helium $fname > /dev/null 2>&1
    echo -n $?,
  else
    timeout 60 helium $fname > /dev/null 2>&1
    echo -n $?,
  fi
  success_count=`cat /tmp/helium/log/rate.txt | grep "compile success" | wc -l`
  error_count=`cat /tmp/helium/log/rate.txt | grep "compile error" | wc -l`
  if [[ $success_count == '0' && $error_count == '0' ]]; then
    buildrate='0'
  else
    buildrate=`echo "scale=2; $success_count/($success_count+$error_count)" | bc`
  fi
  echo $success_count, $error_count, $buildrate
done
