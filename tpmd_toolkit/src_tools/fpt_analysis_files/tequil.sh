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
#Script name: tequil.sh
#
#Purpose: Scans thermal equilibration trajectory file performed prior to TPMD. This analysis is to make sure a stable unique initial #state configuration is feeded to TPMD. List of such valid trajectories is created in this program. If an atomic displacement beyond #vibration tolerance is found during thermal equilibration, such trajectory is discarded from the list. 
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 12 May 2020
#.......................................................................................................................................

cd $glo_pwd/trajectories


#Scan through all trajectories one by one
for ((i=1;i<=$glo_ntraj;i++))
do

cd $i

sed -n -e "10,$glo_blocksize p" $glo_tequil > tmp.0
awk '{print $3" "$4" "$5}' tmp.0 > initial.frame
cat initial.frame >> frame.traj.text
nlin=$(wc -l $glo_tequil | awk '{print $1}') 
lblock=$( expr $nlin \/ $glo_blocksize )

	for ((j=1;j<$lblock;j++))
	do
			#Exiting from whole process when requested  
			exi=$( head -1 $glo_pwd/src_tools/fpt_analysis_files/exit.txt) 
			if [ $exi -eq 1 ]
			then
			echo "Forcefully exiting the program at thermalization stability check " >> $glo_pwd/results/error.txt
			echo "Program has been stopped" > $glo_pwd/results/stopped.txt
			exit
			fi

	startline=$( expr $glo_blocksize \* $j + 10 )

	jjj=$( expr $j + 1 )

	endline=$( expr $glo_blocksize \* $jjj )


	sed -n -e "$startline,$endline p" $glo_tequil > tmp.0
	awk '{print $3" "$4" "$5}' tmp.0 > tmp.1

	cat tmp.1 >> frame.traj.text

	paste initial.frame tmp.1 | awk '{ printf "%4f \t %4f \t %4f \n", $4-$1,$5-$2,$6-$3}' > tmp.diff
	cat tmp.diff | awk -v BoxX=$glo_pbcx -v BoxY=$glo_pbcy -v BoxZ=$glo_pbcz -f $glo_pwd/src_tools/fpt_analysis_files/sign.awk > tmp.disp

	paste initial.frame tmp.disp | awk '{ printf "%4f \t %4f \t %4f \n", $4+$1,$5+$2,$6+$3}' > tmp.unpbc



	paste initial.frame tmp.unpbc | awk '{ printf "%4f \n", sqrt(($4-$1)*($4-$1)+($5-$2)*($5-$2)+($6-$3)*($6-$3))}' | awk '{if ( $1<='$glo_tol') {print 0} else {print 1};}' > $j.frame

	hop=$(grep -n 1 $j.frame | sed -e 's/:/ /')
		if [ "$hop" != "" ]
		then
		thop=1
		break
		else 
		thop=0
		fi


	done

#Listing trajectories valid for TPMD

	if [ $thop -eq 1 ]
	then
	echo $i >> $glo_pwd/results/thermalization/tequilDISCARDlist.txt
	else 
	echo $i >> $glo_pwd/results/thermalization/tequilYESlist.txt
	fi

cd ..
done
