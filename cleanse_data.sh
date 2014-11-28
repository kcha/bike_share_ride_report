#!/bin/bash

FILE=data/ride_data.txt

cat $FILE | \
    sed 's/Ferry Ramp and Queens Quay W/Harbour Square Park/g' | \
    sed 's/Adelaide St/Adelaide St E/g' | \
    sed 's/ \/ / and /g'
