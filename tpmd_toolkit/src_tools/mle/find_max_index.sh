#!/bin/bash


#==============================================================================================
#
#   Copyright 2020 Abhijit Chatterjee
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#==============================================================================================


#```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
#Script name: find_max_index.sh
#
#Purpose: Finds the maximum state index in the file (filein) contaning first passage times from TPMD
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
#
#Date of Modification: 15 May 2020
#.......................................................................................................................................

#Usage: ./find_max_index.sh filein

#Perform initial checks
if [ $#  -eq 0 ]; then
   echo Provide input filename ...
   echo ... usage: ./find_max_index.sh filein
   echo ..     where filein contains the first passage times
   echo ..     collected from TPMD calculations
   exit
fi
filename=$1
awk -v maxindex=0 '{if($1>maxindex){maxindex=$1}}END{print maxindex " number of states found"; print NR-1 " number of first passage times"}' $filename

maxindex=$(awk -v maxindex=0 '{if($1>maxindex){maxindex=$1}}END{print maxindex}' $filename)

#rm tmp.2t824b23f
#echo ""
#echo printing number of transitions for states
#echo State NumberTransitions
#for istate in `seq 1 $maxindex`
#do
#n=$(awk -v is=$istate '{if($1==is){c=c+1}}END{print c}' $filename)
#echo $istate $n >> tmp.2t824b23f
#done
#sort -k2 -n -r tmp.2t824b23f
#rm tmp.2t824b23f
