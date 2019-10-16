#!/bin/bash

docker run --rm -v /home/jjl25/tickviewer:/home/opam/tickviewer -v nb-grader-exchange:/exchange -v /home/caelum:/home/caelum hub /home/opam/tickviewer/exec.sh > /dev/null
