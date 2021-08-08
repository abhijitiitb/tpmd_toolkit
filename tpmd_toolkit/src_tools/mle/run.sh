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
#Script name: run.sh
#
#Purpose: main file used for analyzing TPMD data
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
#
#Date of Modification: 15 May 2020
#.......................................................................................................................................

cd $glo_pwd/src_tools/mle
clear
chmod +x print_fpt_input.sh
chmod +x find_max_index.sh
chmod +x compile.sh
chmod +x frequently_visited_states.sh

skipstep=0

filename=$glo_pwd/results/statestat.txt

################ STEP 1 ################
if [ $skipstep -lt 1 ]; then

   echo '"'$filename'"' " input file containing TPMD FPT data" > fpt.input
   ./find_max_index.sh $filename >> fpt.input
   ./frequently_visited_states.sh $filename
   
   #print information collected in fpt.input
   ./print_fpt_input.sh
   
   #ask user if the information collected is correct
   result=F
   while [ $result = F ]; do
      read -p "Confirm above information is correct [y/n] or exit[e]?" result
      if [ $result == 'y' ] || [ $result == 'Y' ]; then
         echo "Proceeding to next step"
         echo " "      
      elif [ $result == 'n' ] || [ $result == 'N' ]; then
         echo "Modify the file fpt.input manually "
         echo "Skip step 1 and run as "
         echo "./run.sh " $filename " 1"
         exit
      elif [ $result == 'e' ] || [ $result == 'E' ]; then
         exit
      else
         echo "Input can be y/n/e ... try again"
         result=F
      fi
   done
fi
   
################ STEP 2 ################
if [ $skipstep -lt 2 ]; then
   echo "Provide the value of following TPMD parameters..."
   read -p "Stage duration tau (in ps):" tau
   read -p "Temperature in stage 0 (initial stage, in K):" T0 
   read -p "Temperature step (in K):" dT
   read -p "Maximum temperature (in K):" Tmax
   read -p "Index for initial state (typically 0/1):" initialstate
   
   ##Update fpt.input
   echo $tau "Stage duration tau (in ps)" >> fpt.input
   echo $T0 "Temperature in stage 0 (initial stage, in K)" >> fpt.input
   echo $dT "Temperature step (in K)" >> fpt.input
   echo $Tmax "Maximum temperature (in K)" >> fpt.input
   echo $initialstate " initial state idnex (typically 0/1)" >> fpt.input
   
   clear
   #print information collected in fpt.input
   ./print_fpt_input.sh
   
   #ask user if the information collected is correct
   result=F
   while [ $result = F ]; do
      read -p "Confirm above information is correct [y/n] or exit[e]?" result
      if [ $result == 'y' ] || [ $result == 'Y' ]; then
         echo "Proceeding to next step"
         echo " "      
      elif [ $result == 'n' ] || [ $result == 'N' ]; then
         echo "Modify the file fpt.input manually "
         echo "Skip step 1 and run as "
         echo "./run.sh " $filename " 2"
         exit
      elif [ $result == 'e' ] || [ $result == 'E' ]; then
         exit
      else
         echo "Input can be y/n/e ... try again"
         result=F
      fi
   done
fi   

################ STEP 3 ################

if [ $skipstep -lt 3 ]; then
   cp fpt.input fpt.input.bkup
   echo "Provide the number of states to be included/excluded..."
   echo " if states are to be included in group, the number should be positive "
   echo " if states are to be excluded from group, the number should be negative"
   N=0
   while [ $N -eq 0 ]; do
      read -p "Number to states be included/excluded: " N
      if [ $N -lt 0 ]; then
         echo "Number of states to be excluded: " $N
      elif [ $N -gt 0 ]; then
         echo "Number of states to be included: " $N
      else
         read -p "Number of states to be included/excluded cannot be zero.. try again (y/n): " repeat
         if [ $repeat == n ] || [ $repeat == N ]; then
            mv fpt.input.bkup fpt.input
            exit
         fi
      fi
   done
   echo $N "number of states to be included/excluded" >> fpt.input
   
   i=0
   echo " "
   echo " Provide the list of state indices to be included/excluded"
   while [ $i -lt $N ]; do
      i=$(($i+1))
      index=0
      while [ $index -le 0 ]; do
         read -p "$i. - State index:" index 
         if [ $index -le 0 ]; then
            read -p "State index cannot be less than or equal to zero .. try again (y/n): " repeat
            if [ $repeat == n ] || [ $repeat == N ]; then
               mv fpt.input.bkup fpt.input
               exit
            fi
            index=0
         fi
      done
      echo $index " final state index ("$i")" >> fpt.input
      rm fpt.input.bkup
   done
fi
   
################ STEP 4 ################

echo "Process began... please check ./results/mle.txt that will be created soon for MLE results"

./compile.sh
./tpmd-analysis.x > $glo_pwd/results/mle.txt 
#rm fpt.input
cd $glo_pwd


