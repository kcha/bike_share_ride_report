#!/bin/bash

FILE=data/ride_data.txt

cat $FILE | \
    sed 's/ \/ / and /g' | \
    sed 's/Ferry Ramp and Queens Quay W/Harbour Square Park/g' | \
    sed 's/Adelaide St( E)*/Adelaide St E/g'
