#!/bin/bash

mkdir -p output
cat input/airlink.csv | grep -v fleetnumber > output/buses.csv
#rm output/airlinkoneday.csv
#cat input/Stops-UK.csv | grep '\.' | cut -f 30,31 -d "," > output/stops-t.csv
#paste -d "," <(cut -f 2 -d "," output/stops-t.csv) <(cut -f 1 -d "," output/stops-t.csv) > output/stops.csv
#rm output/stops-t.csv
grep '\(stop_id\|latitude\|longitude\)' input/service100.json |tr -d " "| tr -d '\015'|xargs -n3|grep -v stop_id:null |grep -v stop_id:36232426|grep -v stop_id:36232493| cut -d ":" -f 3-| tr ":" "," | cut -d "," -f 1,3 | sort | uniq > output/stops.csv
grep '\(stop_id\|latitude\|longitude\)' input/service100.json.airport |tr -d " "| tr -d '\015'|xargs -n3|grep -v stop_id:null |grep -v stop_id:36232426|grep -v stop_id:36232493| cut -d ":" -f 3-| tr ":" "," | cut -d "," -f 1,3 | sort | uniq > output/stops-airport.csv
grep '\(stop_id\|latitude\|longitude\)' input/service100.json.centre |tr -d " "| tr -d '\015'|xargs -n3|grep -v stop_id:null |grep -v stop_id:36232426|grep -v stop_id:36232493| cut -d ":" -f 3-| tr ":" "," | cut -d "," -f 1,3 | sort | uniq > output/stops-centre.csv
