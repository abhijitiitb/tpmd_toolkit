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
#Script name: vmd.sh
#
#Purpose: Creates a VMD visualization trajectory file for two frames. One is the first frame of the trajectory and the second is the frame of user's choice. This will help to study changes from initital state to new state of configuration through visualisation. 
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 11 May 2020
#.......................................................................................................................................

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~Generating essential directories and taking inputs

if [ ! -d "$glo_pwd/results" ]
then
mkdir $glo_pwd/results
fi

if [ ! -d "$glo_pwd/results/final_state_vis" ]
then
mkdir $glo_pwd/results/final_state_vis
fi

read -p "Choose trajectory [type trajectory number]:" traj
read -p "Please provide the final frame number=" finalframe

foldername=$traj.$finalframe
echo "Process began...please wait till directory with name: $foldername is created in results/final_state_vis directory"


cp $glo_pwd/trajectories/$traj/$glo_trajfile $glo_pwd/src_tools/vmd/

cd $glo_pwd/src_tools/vmd


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~Collecting first and second frame

if [ ! -d "$glo_pwd/src_tools/vmd/VMD" ]
then
mkdir $glo_pwd/src_tools/vmd/VMD
fi


blocksize=$(($glo_natom+9)) #block size in lammps output trajectory

fframe=$( expr $blocksize \* $finalframe + 1 )
lframe=$( expr $blocksize \* $finalframe + $blocksize )

sed -n "1,$blocksize p" $glo_trajfile > VMD/vmd.lammpstrj
sed -n "$fframe,$lframe p" $glo_trajfile >> VMD/vmd.lammpstrj

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~saving final result in results directory


cp -r $glo_pwd/src_tools/vmd/VMD $glo_pwd/results/final_state_vis
		
		if [ -d "$glo_pwd/results/final_state_vis/$foldername" ]
		then
		rm -r $glo_pwd/results/final_state_vis/$foldername
		fi

mv $glo_pwd/results/final_state_vis/VMD $glo_pwd/results/final_state_vis/$foldername
mv $glo_pwd/results/final_state_vis/$foldername/vmd.lammpstrj  $glo_pwd/results/final_state_vis/$foldername/$foldername.lammpstrj

rm $glo_trajfile
cd $glo_pwd
