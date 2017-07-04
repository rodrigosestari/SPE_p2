#!/bin/sh

for i in $(seq 0 $1); do
    ./main.py -c config.json -r $i
done
