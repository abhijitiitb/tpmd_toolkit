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
#Script name: start.sh
#
#Purpose: Takes inputs from parameter.txt file and exports it as global variable which will be used by all the child processes. Executes the process opted by user through interactive terminal interface.
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 15 May 2020
#.......................................................................................................................................

#~~~~~~~~~~~~~~ Reading the parameters from parameter.txt


#Variable starting with 'glo_' are globally comman variable

echo "Reading Parameters..."
glo_pwd=$PWD
export glo_pwd
export parameter=$glo_pwd/parameters.txt

glo_system=$( sed -n " 3 p " $parameter | sed -e 's/System name=//')
glo_trajfile=$( sed -n " 4 p " $parameter | sed -e 's/Name of trajectory file=//')
glo_tequil=$( sed -n " 5 p " $parameter | sed -e 's/Name of thermal equilibration trajectory file=//')
glo_ntraj=$( sed -n " 6 p " $parameter | sed -e 's/Number of trajectories=//')
glo_nrow=$( sed -n " 7 p " $parameter | sed -e 's/Number of parallel processes=//')
glo_natom=$( sed -n " 8 p " $parameter | sed -e 's/Total number of atoms in the system=//')
glo_nfpt=$( sed -n " 9 p " $parameter | sed -e 's/Minimum number of consequtive frames for transition validation=//')
glo_dist=$( sed -n " 10 p " $parameter | sed -e 's/Tolerance for transition(distance unit)=//')
glo_tol=$( sed -n " 11 p " $parameter | sed -e 's/Tolerance for new state(distance unit)=//')
glo_pbcx=$( sed -n " 12 p " $parameter | sed -e 's/Size of box x-direction(distance unit)=//')
glo_pbcy=$( sed -n " 13 p " $parameter | sed -e 's/Size of box y-direction(distance unit)=//')
glo_pbcz=$( sed -n " 14 p " $parameter | sed -e 's/Size of box z-direction(distance unit)=//')
glo_nframe=$( sed -n " 15 p " $parameter | sed -e 's/Number of frames in each trajectory=//')
glo_time=$( sed -n " 16 p " $parameter | sed -e 's/Time duration between two subsequent frames(time unit)=//')
glo_limit=$( sed -n " 17 p " $parameter | sed -e 's/Number of times to allow drift of atom=//')
#glo_tempstep=$( sed -n " 18 p " $parameter | sed -e 's/Number of temperature steps in TPMD=//')
#glo_stepperiod=$( sed -n " 17 p " $parameter | sed -e 's/Duration of temperature step(time unit)=//')

glo_ttime=$( echo "$glo_time*$glo_nframe" | bc)
glo_blocksize=$(($glo_natom+9))

#~~~~~~~~~~~~~~ Exporting parameters as global varables
export glo_nrow
export glo_ntraj
export glo_trajfile
export glo_natom
export glo_nfpt
export glo_tol
export glo_dist
export glo_pbcx
export glo_pbcy
export glo_pbcz
export glo_nframe
export glo_time
export glo_ttime
export glo_tequil
export glo_limit
export glo_blocksize
#export glo_tempstep
#export glo_stepperiod
export glo_system

echo "System name:$glo_system"
echo "Key Parameters:"
echo "#Transition tolerance=$glo_tol,#New state tolerance=$glo_dist,#Min. consequent frames=$glo_nfpt" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Check if required trajector files are present~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		#Make sure the required directories and trajectory files are present
		if [ ! -d "trajectories" ]
		then
		mkdir $glo_pwd/trajectories
		read -n 1 -s -r -p  "Make sure your trajectories are placed and arranged with correct numbering in directory named 'trajectories'. Press any key to continue. "
		fi

		if [ -z "$( ls -A $glo_pwd/trajectories)" ]
		then
		echo "Trajectories directory is empty, can not proceed."
		exit
		fi

if [ ! -e $glo_pwd/trajectories/1/$glo_trajfile ]
then
echo "TPMD trajecotry file mentioned in parameters.txt is not present. Can not proceed."
exit
fi

if [ ! -e $glo_pwd/trajectories/1/$glo_tequil ]
then
echo "Thermalizatoin trajecotry file mentioned in parameters.txt is not present. Can not proceed."
exit
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Interactive interface on terminal~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

echo "Choose the operation"
echo "[1] Transition detection"
echo "[2] State indexing"
echo "[3] Maximum likelihood estimation (MLE)"
echo "[4] Displacement v/s frame plot"
echo "[5] Visualizing initial and final frame" 
echo "[6] NEB calculation input file for LAMMPS"
read -p "Submit the option number accordingly:" ans

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[1] Transition detection and mechanism file generation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if [ $ans == "1" ]
then

		#Clean the previous data for new calculations and results
		if [ -e trajs ] || [ -e results ]
		then
		read -n 1 -s -r -p "The old 'results' or 'trajs' directory will be overwritten. Press any key to continue." answer
			if [ -e trajs ]	
			then
			rm -r trajs
			fi
			if [ -e results ]
			then
			rm -r results
			fi
		fi

		if [ -e row* ]
		then
		rm -r row*
		fi

	mkdir $glo_pwd/results

	#Call forth the FPT data collection function
	chmod +x $glo_pwd/src_tools/fpt_analysis_files/fptbegin.sh
	$glo_pwd/src_tools/fpt_analysis_files/fptbegin.sh &> $glo_pwd/results/error.txt &
	echo ""
	echo "Process began..."
	echo "Please check ./results/log.txt file for status of process"
	echo "To exit from process change ./src_tools/fpt_analysis_files/exit.txt file text from '0' to '1' and save the file. "
	echo 0 > $glo_pwd/src_tools/fpt_analysis_files/exit.txt #Required for exit mechanims in process

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[2] State indexing and FPT collection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elif [ $ans == "2" ]
then
	
	echo "Process began..."
	chmod +x $glo_pwd/src_tools/state_indexing/begin.sh
	$glo_pwd/src_tools/state_indexing/begin.sh  &>> $glo_pwd/results/error.txt &
        echo "Please check ./results/log.txt file for status of process"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[3] Maximum likelihood estimation (MLE)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elif [ $ans == "3" ]
then
	

	chmod +x $glo_pwd/src_tools/mle/run.sh
	$glo_pwd/src_tools/mle/run.sh
	

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[4] Displacement v/s frame plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elif [ $ans == "4" ]
then
	chmod +x $glo_pwd/src_tools/dispVSframe/input.sh
	$glo_pwd/src_tools/dispVSframe/input.sh


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[5] Visualizing initial and final frame~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


elif [ $ans == "5" ]
then
	chmod +x $glo_pwd/src_tools/vmd/vmd.sh
	$glo_pwd/src_tools/vmd/vmd.sh



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[6] NEB calculation input file for LAMMPS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


elif [ $ans == "6" ]
then
	chmod +x $glo_pwd/src_tools/neb/input.sh
	$glo_pwd/src_tools/neb/input.sh

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~for invalid input~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
else
	echo "Invalid option, exiting program"
	exit
fi
