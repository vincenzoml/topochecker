#!/bin/bash

mkdir -p output
cat input/airlink.csv | grep -v fleetnumber > output/buses.csv
#rm output/airlinkoneday.csv
cat input/Stops-UK.csv | grep '\.' | cut -f 30,31 -d "," > output/stops-t.csv
paste -d "," <(cut -f 2 -d "," output/stops-t.csv) <(cut -f 1 -d "," output/stops-t.csv) > output/stops.csv
#rm output/stops-t.csv


