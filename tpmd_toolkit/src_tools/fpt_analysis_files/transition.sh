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
#Script name: transition.sh
#
#Purpose: For each requested atom, it finds the displacement of atom from initial position in each frame throughout trajectory. Detects #the frame where atom is displaced beyond distance limit. Detects the type of displacement of atom, whether it is hopped to stable #position or drifting continously. Registers the information (frame of hop, atom index and type of displacement) of each displaced atom #in a list called 'transition list'.
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 13 August 2019
#.......................................................................................................................................

#Store first frame atom coordiantes indexed serially in ascending order of atomic index in trajectory
startline=$(($lclv_iatom+9)) 
sed -n "${startline}~${glo_blocksize}p" $glo_pwd/trajectories/$lclv_itraj/$glo_trajfile > tmp.0
awk '{print $3" "$4" "$5}' tmp.0 > tmp.1  #LAMMPS coulumn 3,4,5 belong to x,y,z coordinate positions
$lclv_pwd/pbc.sh tmp.1 $glo_pbcx $glo_pbcy $glo_pbcz tmp.2 #Un-PBC the coordinates

#Calculate the displacement of an atom from initial position throughout trajectory

function_displacement(){

txt=$( sed -n "1 p" tmp.2 | awk '{print $1" "$2" "$3}')
x0=$(echo $txt | awk '{print $1}')
y0=$(echo $txt | awk '{print $2}')
z0=$(echo $txt | awk '{print $3}')

awk -v x0=$x0 -v y0=$y0 -v z0=$z0 \
'{sqdisp=($1-x0)*($1-x0)+($2-y0)*($2-y0)+($3-z0)*($3-z0); printf "%4f \n", sqdisp}' tmp.2 \
> sqdisp.txt			

if [ -e tmp.txt ]
then
rm tmp.txt			
fi

for((ll=1;ll<=$glo_nframe;ll++))
do
  sqdispcopy=$( sed -n "$ll p" sqdisp.txt | awk '{ print $1}')
  disp=$(echo "sqrt($sqdispcopy)" | bc)
  echo $disp" "$ll >>tmp.txt
done

}

rfpt=$( expr $glo_nfpt - 1) #To satisfy number of consecutive frame condition required to validate as transition


#Get the frame where atom hops first
function_binarydetect(){

fileout=$lclv_iatom.txt	

check1=$(cat tmp.txt| awk '{if ( $1>='$glo_dist') {print 1};}' | awk '{sum+=$1} END {if (sum == 0) {print 0} else {print sum}}')

if [ $check1 -gt $rfpt ] 
then

  cat tmp.txt| awk '{if ( $1<='$glo_dist') {print 0} else {print 1};}' > $fileout

  cat tmp.txt> $lclv_iatom.disp
  frames1=$(grep -n 1 $fileout | sed -e 's/:/ /')
  lclv_1sthopframe=$(echo $frames1 | awk '{print $1}')
  export lclv_1sthopframe
else
	exit 
fi
}

#Check the displacement type
function_detectfpt(){

fileout=$lclv_iatom.txt
limit=1 	


for((i=$lclv_1sthopframe;i<=$glo_nframe;i++))
do	

frame2=$( expr $i + $rfpt )

check2=$(cat $fileout |  sed -n "$i,$frame2 p"  | awk '{sum+=$1} END {print sum}')
export lclv_pass1=0

	if [ $check2 -gt $rfpt ]
	then 
		if [ $limit -eq 1 ]
		then
	  	lclv_1stconti=$i
		fi

	disp1=$( sed -n "$i p" tmp.txt | awk '{ print $1}')
	disp11=$( echo "$disp1 - $glo_tol" |bc )
	disp12=$( echo "$disp1 + $glo_tol" |bc )
	frame3=$( expr $i + 1 )
	good=1
		for((j=$frame3;j<=$frame2;j++))
		do

		disp2=$( sed -n "$j p" tmp.txt | awk '{ print $1}')

			if [ `echo "$disp2>$disp11"|bc` -eq 1 ] && [ `echo "$disp2<$disp12"|bc` -eq 1 ]; then

 			good=$( expr $good + 1 )

			fi	
		done

		if [ $good -gt $rfpt ]
		then

		export lclv_pass1=1
		export lclv_tf=$i
		break

		else
		
		limit=$( expr $limit + 1 )
			
			if [ $limit -gt $glo_limit ]
			then 
			export lclv_pass1=2
			export lclv_tf=$lclv_1stconti
			break
			fi

		fi


	fi
done

}

function_displacement
function_binarydetect
function_detectfpt

#Create 'transition list' of atoms hopped during transition with information (frame of hop, atom index and type of displacement)
if [ $lclv_pass1 -eq 1 ]
then
echo $lclv_tf $lclv_iatom" "1" "#hop_frame atom_number displacement_type >> tmp.trans 
elif [ $lclv_pass1 -eq 2 ]
then
echo "$lclv_itraj trajectory: Movement of atom number: $lclv_iatom is not stable" >> $glo_pwd/results/warning.txt   
echo $lclv_tf $lclv_iatom" "2" "#hop_frame atom_number displacement_type >> tmp.trans 
fi







