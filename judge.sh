#!/bin/bash
(conda run -n judge-environment --no-capture-output --live-stream python main.py $1 $2 $3 $4 $5) &> /home/amirhosein/judge/$1.log
