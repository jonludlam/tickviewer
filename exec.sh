#!/bin/bash

opam install -y lwt calendar astring
cd /home/opam/tickviewer/
dune exec ./tickviewer.exe /exchange/FoCS/inbound
