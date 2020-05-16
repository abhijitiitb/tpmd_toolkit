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
#Script name: input.sh
#
#Purpose: Collects the inputs from user for processing their choice of trajectory, frame and atoms.
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 11 May 2020
#.......................................................................................................................................

	read -p "Choose trajectory [type trajectory number]:" traj
	echo "Choose atom range" 
	read -p "First atom index number:" atom1
	read -p "Last atom index number:" atom2
	read -p "Please provide the final frame=" frametime

	foldername=$traj.$atom1.$atom2.neb
	filename=$traj.$atom1.$atom2
	
	export atom1
	export atom2
	export traj
	export frametime
	export foldername
	export filename

		if [ ! -d "results" ]
		then
		mkdir $glo_pwd/results
		fi

		cd $glo_pwd/results/
		if [ ! -d "NEBinput" ]
		then
		mkdir $glo_pwd/results/NEBinput
		fi
		cd $glo_pwd

	cp $glo_pwd/trajectories/$traj/$glo_trajfile $glo_pwd/src_tools/neb/

	echo "process began...please wait till directory is generated in results/NEBinput with name: $foldername "
	chmod +x $glo_pwd/src_tools/neb/neb.sh 
	$glo_pwd/src_tools/neb/neb.sh &
	
#	echo "Check the plot in /results/NEBinput with directory name: $foldername"
	
