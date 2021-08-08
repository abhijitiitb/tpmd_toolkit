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
#Script name: state.sh
#
#Purpose: Compares mechanism files of each trajectory one by one with ones in mechanism database to assign state indexes. Creates state #index vs fpt data essential for TPMD rate calculations. Updates mechanism database for new found state. 
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 05 August 2021
#.......................................................................................................................................


rm $glo_pwd/results/allmechanisms/* 
rm $glo_pwd/results/mechanisms/*
mkdir $glo_pwd/results/mechanisms #mechanism database
echo 0 > $glo_pwd/results/mechanisms/0.state
echo 0 >> $glo_pwd/results/mechanisms/0.state
rm $glo_pwd/results/statestat.txt
cp -r $glo_pwd/trajs/allmechanisms $glo_pwd/trajs/allmechanisms_hold
echo "#State_index      FPT(timeunit)   Trajectory_number       Atom number(s) #Vibration_Tolerance=$glo_tol,#Distance_limit=$glo_dist,#Frame_condition=$glo_nfpt"  > $glo_pwd/results/statestat.txt

#Go through all mechanism files one by one to assign the state index
sort -k1 -n $glo_pwd/trajs/mechanism.list -o $glo_pwd/trajs/mechanism.list 
ntraj=$(wc -l $glo_pwd/trajs/mechanism.list | awk '{print $1}') 


for((f=1;f<=$ntraj;f++))
do


			#Exiting from whole process when requested  
			exi=$( head -1 $glo_pwd/src_tools/fpt_analysis_files/exit.txt) 
			if [ $exi -eq 1 ]
			then
			echo "Forcefully exiting the program at state indexing step. " >> $glo_pwd/results/error.txt
			echo "Program has been stopped" > $glo_pwd/results/stopped.txt
			exit
			fi



found=0
g=$( sed -n "$f p" $glo_pwd/trajs/mechanism.list | awk '{print $1}' ) #trajectory number
n=$( head -1 $glo_pwd/src_tools/state_indexing/scounter.txt) #State index counter

fpt=$(tail -1 $glo_pwd/trajs/allmechanisms/$g.txt | awk '{print $1}') #FPT
aa_atoms=$( sed -n "2 p" $glo_pwd/trajs/allmechanisms/$g.txt  | awk '{ print $0}') 


#Compare the mechanism file of trajectory with distinct mechanisms stored in database
	for((i=0;i<=$n;i++))
	do

	#number of atoms involved in mechanism
	snatom=$( sed -n "2 p" $glo_pwd/results/mechanisms/$i.state  | awk '{ print $1}') #in mechanism file of database
	ntatom=$( sed -n "1 p" $glo_pwd/trajs/allmechanisms/$g.txt  | awk '{ print $1}') #in trajectory mechanism file yet to be indexed

		#For no transition during finite time TPMD, assigning it state zero
		if [ $ntatom -eq 0 ]
		then
			
			echo 0 $glo_ttime $g>> $glo_pwd/results/statestat.txt
			sed -i '1i'0'  :state index(mechanism type) \' $glo_pwd/trajs/allmechanisms/$g.txt 		
			cp $glo_pwd/trajs/allmechanisms/$g.txt $glo_pwd/results/allmechanisms/$g.0.state 
			found=1
			break
		#Compare number of atoms involved in mechanism
		elif [ $snatom -eq $ntatom ]
		then
		
		a_atoms=$( sed -n "3 p" $glo_pwd/results/mechanisms/$i.state  | awk '{ print $0}') 

			#Compare the atoms involved in the mechanism
			if [ "$a_atoms" == "$aa_atoms" ]
			then
			atomline=$( expr 2 + $snatom )
			good=1
				
				#Compare atomic coordinates 
				for((j=3;j<=$atomline;j++))
				do
				jj=$(expr $j + 1 )
							
				txt1=$( sed -n " $j p " $glo_pwd/trajs/allmechanisms/$g.txt | awk '{print $1" "$2" "$3" "$4" "$5}')
				x1=$(echo $txt1 | awk '{print $3}')
				y1=$(echo $txt1 | awk '{print $4}')
				z1=$(echo $txt1 | awk '{print $5}')

				txt2=$( sed -n " $jj p " $glo_pwd/results/mechanisms/$i.state | awk '{print $1" "$2" "$3" "$4" "$5}') 
				x2=$(echo $txt2 | awk '{print $3}')
				y2=$(echo $txt2 | awk '{print $4}')
				z2=$(echo $txt2 | awk '{print $5}')

				xl=$(echo "$x2-$glo_tol" | bc)
				yl=$(echo "$y2-$glo_tol" | bc)
				zl=$(echo "$z2-$glo_tol" | bc)

				xh=$(echo "$x2+$glo_tol" | bc)
				yh=$(echo "$y2+$glo_tol" | bc)
				zh=$(echo "$z2+$glo_tol" | bc)

					if [ `echo "$xh>$x1"|bc` -eq 1 ] && [ `echo "$xl<$x1"|bc` -eq 1 ] && [ `echo "$yh>$y1"|bc` -eq 1 ] && [ `echo "$yl<$y1"|bc` -eq 1 ] && [ `echo "$zl<$z1"|bc` -eq 1 ] && [ `echo "$zh>$z1"|bc` -eq 1 ]  ; then

					good=$( expr $good + 1 )

					fi
				done
				
	
				#When match is found in database, assign that state index and register it in FPT v/s state index data
				if [ $good -gt $snatom ]
				then
				echo $i $fpt $g $aa_atoms>> $glo_pwd/results/statestat.txt
				sed -i '1i'$i'  :state index(mechanism type) \' $glo_pwd/trajs/allmechanisms/$g.txt 		
				cp $glo_pwd/trajs/allmechanisms/$g.txt $glo_pwd/results/allmechanisms/$g.$i.state 
				found=1
				break
				fi

			fi
		fi

	done

	#When match is not found in database, update database with new state
	if [ $found -eq 0 ]
	then
	n=$( expr $n + 1 )
	echo $n > $glo_pwd/src_tools/state_indexing/scounter.txt
	echo $n $fpt $g $aa_atoms>> $glo_pwd/results/statestat.txt
	sed -i '1i'$n'  :state index(mechanism type) \' $glo_pwd/trajs/allmechanisms/$g.txt
	cp $glo_pwd/trajs/allmechanisms/$g.txt $glo_pwd/results/mechanisms/$n.state
	cp $glo_pwd/trajs/allmechanisms/$g.txt  $glo_pwd/results/allmechanisms/$g.$n.state 
	fi

done

rm -r $glo_pwd/trajs/allmechanisms
mv $glo_pwd/trajs/allmechanisms_hold $glo_pwd/trajs/allmechanisms
