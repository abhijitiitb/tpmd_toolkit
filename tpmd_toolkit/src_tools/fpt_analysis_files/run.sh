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
#Purpose: Runs distributed trajectories in single process one by one through various scans. Registers special cases in output: (1) If no #transition is found, (2) If atoms are drifiting in trajectory during transition. Keeps count of number of parallel processes completed #and call forths state detection, rate calculation and other post analysis processes. 
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 13 August 2019
#.......................................................................................................................................

lclv_pwd=$PWD
export lclv_pwd

#Detect the trajectories to be scanned
for j in `ls -d */`;do b=$(basename $j /); echo $b>>folder.list;done

#Run through each trajectory one by one
if [ -e folder.list ]
then
	for i in $(cat folder.list)
	do
	  cd $lclv_pwd/${i}
	#  echo $i = trajectory ongoing
	  export lclv_itraj=$i

			#Exiting from whole process when requested
		  	exi=$( head -1 $glo_pwd/src_tools/fpt_analysis_files/exit.txt) 
			if [ $exi -eq 1 ]
			then
			echo "Forcefully exiting the program at transition detection test, at trajecotry $i " >> $glo_pwd/results/error.txt
			cat $lclv_pwd/*.error >> $glo_pwd/results/error.txt
			echo "Program has been stopped" > $glo_pwd/results/stopped.txt
			exit
			fi


	#Do framewise scan
	$lclv_pwd/framescan.sh 

	  if [ -e tmp.trans ]
	  then
	    #Call program to create mechanism file 
	    $lclv_pwd/fpt.sh 
	 	if [ -e trans.fpt ]
	  	then
		
		#Register the index and save mechanism file of trajectories where transition is observed
		echo "$i" >> $glo_pwd/trajs/mechanism.list
		cat trans.fpt > $glo_pwd/trajs/allmechanisms/$i.txt

	  	else

		  echo ""$i" trajectory : No stable change of configuration is obsereved" >> $glo_pwd/results/warning.txt

		fi

	  else 

		#Register the index and save state zero mechanism file in case transition is not observed
		echo "$i" >> $glo_pwd/trajs/mechanism.list
		echo 0 > $glo_pwd/trajs/allmechanisms/$i.txt #state index
		echo 0 >> $glo_pwd/trajs/allmechanisms/$i.txt #number of atoms 

	  fi
	  cd $lclv_pwd

	done
cat $lclv_pwd/*.error >> $glo_pwd/results/error.txt
fi


#Increment the count as the process of one thread is done
echo "done" > $glo_pwd/trajs/done/lclv.txt
pcc=$(ls -l $glo_pwd/trajs/done | grep ^- | wc -l)
echo "Thread completed:$pcc" >> $glo_pwd/results/log.txt

if [ $pcc -eq $glo_nrow ]
then

echo "" >> $glo_pwd/results/log.txt
echo "Transition detection completed." >> $glo_pwd/results/log.txt
date >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt

fi



