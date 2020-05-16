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
#Purpose: Collects the inputs from user for processing their choice of trajectory, frame and atoms. Generates required direcotries for process.
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 11 May 2020
#.......................................................................................................................................

#	cp $glo_pwd/parameters.txt $glo_pwd/src_tools/dispVSframe/
	read -p "Choose trajectory [type trajectory number]:" traj
	read -p "Plot for [1] Single atom (type 1) or [2] Multiple consecutive atoms (type 2):" plot

		if [ $plot == "1" ]
		then
		read -p "Atom index number:" atom
		atom1=$atom
		atom2=$atom
		filename=$traj.$atom.dispVSframe.pdf
		elif [ $plot == "2" ]
		then	
		read -p "First atom index number:" atom1
		read -p "Last atom index number:" atom2
		filename=$traj.$atom1.$atom2.dispVSframe.pdf
		else
		echo "Invalid option, exiting program"
		exit
		fi


cp $glo_pwd/trajectories/$traj/$glo_trajfile $glo_pwd/src_tools/dispVSframe/

export traj
export atom1
export atom2
export foldername
export filename

		if [ ! -d "$glo_pwd/results" ]
		then
		mkdir $glo_pwd/results
		fi

		cd $glo_pwd/results/
		if [ ! -d "$glo_pwd/results/plots" ]
		then
		mkdir $glo_pwd/results/plots
		fi
		cd $glo_pwd



echo "process began...please wait till file is generated in results/plots with name: $filename "
chmod +x $glo_pwd/src_tools/dispVSframe/dispVSframe.sh
$glo_pwd/src_tools/dispVSframe/dispVSframe.sh &
	
#	echo "Check the plot in /results/NEBinput with directory name: $foldername"
	
