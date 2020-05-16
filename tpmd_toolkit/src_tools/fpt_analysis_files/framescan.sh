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
#Script name: framescan.sh
#
#Purpose: Scans the trajectory frame by frame until a long lived new state (transition) is detected. If the new long-lived state is #present, it invokes atom wise scan program(transition.sh) for each atom displaced during transition.   
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 13 August 2019
#.......................................................................................................................................

#Store first frame atom coordiantes indexed serially in ascending order of atomic index in trajectory
sed -n -e "10,$glo_blocksize p" $glo_pwd/trajectories/$lclv_itraj/$glo_trajfile > tmp.0
awk '{print $3" "$4" "$5}' tmp.0 > initial.frame

good=0
rfpt=$( expr $glo_nfpt - 1 ) #to satisfy number of consecutive frame condition required to validate as transition

#Go through frame by frame
for ((j=1;j<=$glo_nframe;j++))
do


		#Exiting from whole process when requested		
	  	exi=$( head -1 $glo_pwd/src_tools/fpt_analysis_files/exit.txt) 
		if [ $exi -eq 1 ]
		then
		echo "Forcefully exiting the program at trajecotry $lclv_itraj " >> $glo_pwd/results/error.txt 
		echo "Program has been stopped" > $glo_pwd/results/stopped.txt
		exit
		fi



#Extract and unpbc the coordinates of j^th frame
startline=$( expr $glo_blocksize \* $j + 10 )
jjj=$( expr $j + 1 )
endline=$( expr $glo_blocksize \* $jjj )
sed -n -e "$startline,$endline p" $glo_pwd/trajectories/$lclv_itraj/$glo_trajfile > tmp.0
awk '{print $3" "$4" "$5}' tmp.0 > tmp.1
paste initial.frame tmp.1 | awk '{ printf "%4f \t %4f \t %4f \n", $4-$1,$5-$2,$6-$3}' > tmp.diff
cat tmp.diff | awk -v BoxX=$glo_pbcx -v BoxY=$glo_pbcy -v BoxZ=$glo_pbcz -f $lclv_pwd/sign.awk > tmp.disp
paste initial.frame tmp.disp | awk '{ printf "%4f \t %4f \t %4f \n", $4+$1,$5+$2,$6+$3}' > tmp.unpbc

#Check if any atom is displaced in j^th frame
paste initial.frame tmp.unpbc | awk '{ printf "%4f \n", sqrt(($4-$1)*($4-$1)+($5-$2)*($5-$2)+($6-$3)*($6-$3))}' | awk '{if ( $1<='$glo_dist') {print 0} else {print 1};}' > $j.frame
hop=$(grep -n 1 $j.frame | sed -e 's/:/ /')

jj=$( expr $j - 1 )

#If atom is found hopped, compare the j^th frame with previous frame
	if [ "$hop" != "" ] && [ $jj -gt 0 ]
	then
	compare=$( cmp $jj.frame $j.frame )

		if [ "$compare" == "" ]
		then
		good=$( expr $good + 1 )

			#If the new long lived state is found
			if [ $good -eq $rfpt ]
			then
			cat $j.frame > hop.fptframe
			rm *.frame
			break
			fi
		else
		good=0
		fi
	else
	good=0
	fi
done

#Do atomwise scan for atoms displaced beyond displacement limit
if [ -e hop.fptframe ]
then
	for ((k=1;k<=$glo_natom;k++))
	do
	binaryhop=$( sed -n " $k p " hop.fptframe | awk '{ print $1}')
		if [ $binaryhop -eq 1 ]
		then
		lclv_iatom=$k
		export lclv_iatom
		$lclv_pwd/transition.sh
		fi
	done
fi
