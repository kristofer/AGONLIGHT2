#!/bin/bash
set verbose on
cp template.dsk newcpmk.dsk
cpmcp -fnihirash newcpmk.dsk DriveK/*.* 0:
mv newcpmk.dsk cpmk.dsk
cp cpmk.dsk /media/kristofer/ADL/cpm
