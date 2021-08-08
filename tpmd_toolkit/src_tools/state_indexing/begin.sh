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
#Script name: begin.sh
#
#Purpose: Initiates the state identification, state indexing, FPT data graph plotting process 
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 05 August 2021
#.......................................................................................................................................

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Post transition detection processes are evoked here~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

echo "" >> $glo_pwd/results/log.txt

echo "~~~~~~~~~~STATE INDEXING~~~~~~~~~~" >> $glo_pwd/results/log.txt

date >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "State indexing began..." >> $glo_pwd/results/log.txt

echo 0 > $glo_pwd/src_tools/state_indexing/scounter.txt 
chmod +x $glo_pwd/src_tools/state_indexing/state.sh
$glo_pwd/src_tools/state_indexing/state.sh

echo "State indexing completed." >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "Check the /results/statestat.txt for state indexing information" >> $glo_pwd/results/log.txt 
echo "Check the /results/error.txt for errors" >> $glo_pwd/results/log.txt 
echo "New states found and thier mechanisms are stored in mechanism/ directory" >> $glo_pwd/results/log.txt
echo "To check the transition mechanism for all the directories refer allmechanism/ directory " >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt

echo "Whole process completed." >> $glo_pwd/results/log.txt
echo "Program stopped." >> $glo_pwd/results/log.txt
date >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt

