#!/bin/bash

for name in data/preprocessed/*; do
    helium-gen-assert-poi.sh $name >> assert.csv;
done

