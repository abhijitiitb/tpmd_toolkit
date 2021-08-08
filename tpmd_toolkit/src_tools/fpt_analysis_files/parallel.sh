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
#Script name: parallel.sh
#
#Purpose: Divides the list of valid trajectories in requested number of parallel processes. Distributes files and directories, creates #distinct global variables for each process and execute those processes parallely.  
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 13 August 2019
#.......................................................................................................................................

#Variable starting with 'lclv_' are locally comman variables within one of parallel process

cd $glo_pwd  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Distribute parallel processes~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Calculate number of trajectories in each parallel process
ntraj=$(wc -l $glo_pwd/results/thermalization/tequilYESlist.txt | awk '{print $1}') 
reminder=$(($ntraj % $glo_nrow))
	
	if [ $reminder -ne 0 ]
	then
	exact=$(( $ntraj - $reminder ))
	perrow=$( expr $exact \/ $glo_nrow )
	else
	perrow=$( expr $ntraj \/ $glo_nrow )
	exact=$(($ntraj - $perrow))
	fi
	echo "$ntraj trajectories are assigned in $glo_nrow parallel processes" >> $glo_pwd/results/log.txt
	
	if [ $reminder -ne 0 ]
	then
	echo "Each parallel process has $perrow trajectories, except last parallel process" >> $glo_pwd/results/log.txt
	lastpar=$( expr $perrow + $reminder )
	echo "Last parallel process has $lastpar trajectores" >> $glo_pwd/results/log.txt
	else
	echo "Each parallel process has $perrow trajectories" >> $glo_pwd/results/log.txt
	fi

#Divide and assign the trajectories to be analyzed among parallel processes 

echo "Assigning the trajectories to each parallel thread" >> $glo_pwd/results/log.txt

for ((a=1;a<=$glo_nrow;a++))
do
		#Exiting from whole process when requested
		exi=$( head -1 $glo_pwd/src_tools/fpt_analysis_files/exit.txt) 
		if [ $exi -eq 1 ]
		then
		echo "Forcefully exiting the program at parallel row $a " >> $glo_pwd/results/error.txt
		echo "Program has been stopped" > $glo_pwd/results/stopped.txt
		exit
		fi

  mkdir $glo_pwd/row$a 


	if [ $a -ne $glo_nrow ]
	then
	  b=$( expr $perrow \* $a )
	else
	  b=$ntraj
	fi

  c=$( expr $perrow - 1 )

	if [ $a -ne $glo_nrow ]
	then
	  d=$( expr $b - $c )
	else
	  d=$( expr $b - $c - $reminder )
	fi

	for ((e=$d;e<=$b;e++))
	do
	f=$( sed -n "$e p" $glo_pwd/results/thermalization/tequilYESlist.txt | awk '{print $1}' )
	mkdir $glo_pwd/row$a/$f
	done

  mv $glo_pwd/row$a $glo_pwd/trajs
done

cd $glo_pwd/src_tools/fpt_analysis_files 

#Distribute processing programs and variable in each parallel process

echo "Distributing program scripts and variables to each parallel thread" >> $glo_pwd/results/log.txt
for ((a=1;a<=$glo_nrow;a++))
do
		#Exiting from whole process when requested		
		exi=$( head -1 ./exit.txt) 
		if [ $exi -eq 1 ]
		then
		echo "Forcefully exiting the program at parallel row $a " >> $glo_pwd/results/error.txt
		echo "Program has been stopped" > $glo_pwd/results/stopped.txt
		exit
		fi

  cp *.sh *.txt *.awk $glo_pwd/trajs/row$a ;
  cd $glo_pwd/trajs/row$a

  sed -i 's/lclv/loc'$a'/g' *.sh   #Naming local variable in each parallel process
  b=$( expr $perrow \* $a )
  c=$( expr $perrow - 1 )
  d=$( expr $b - $c )
  $glo_pwd/trajs/row$a/run.sh &> $glo_pwd/trajs/row$a/$a.error &
  cd $glo_pwd/src_tools/fpt_analysis_files

done

echo "Parallelization complete." >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "~~~~~~~~~~TRANSITION DETECTION~~~~~~~~~~" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "Transition detection begins on $glo_nrow parallel threads." >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
