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
#Script name: fptbegin.sh
#
#Purpose: Creates directories and files required during FPT-TPMD analysis. Calls thermalization behaviour validation program. Starts the #parallelization of transition detection process.  
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 05 August 2021
#.......................................................................................................................................

date > $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "~~~~~~~~~~~~ System name: $glo_system ~~~~~~~~~~~~~~" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt

echo "TPMD-FPT analysis process began..." >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt


#Create required directories and files 
echo "Generating required files and directories..." >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt

mkdir $glo_pwd/trajs
mkdir $glo_pwd/trajs/done #counter for parallel processes
mkdir $glo_pwd/trajs/allmechanisms
mkdir $glo_pwd/results/plots
mkdir $glo_pwd/results/thermalization
mkdir $glo_pwd/results/allmechanisms #mechanisms of all the trajectories
mkdir $glo_pwd/results/mechanisms #mechanism database
echo 0 > $glo_pwd/results/mechanisms/0.state
echo 0 >> $glo_pwd/results/mechanisms/0.state 
#echo "#State_index	FPT(timeunit)	Trajectory_number	Atom number(s) #Vibration_Tolerance=$glo_tol,#Distance_limit=$glo_dist,#Frame_condition=$glo_nfpt"  > $glo_pwd/results/statestat.txt
echo "#Warnings"  > $glo_pwd/results/warning.txt


#Copy parameter file to log file 
echo "~~~~~~~~~~PARAMETERS USED~~~~~~~~~~" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
cat $glo_pwd/parameters.txt >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Analysing thermal equilibration trajectory~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

echo "~~~~~~~~~~THERMALIZATION~~~~~~~~~~" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "Thermalization stability check begins..." >> $glo_pwd/results/log.txt

chmod +x $glo_pwd/src_tools/fpt_analysis_files/tequil.sh
$glo_pwd/src_tools/fpt_analysis_files/tequil.sh 
	
echo "Thermalization stability check ends" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt


ttraj=$(wc -l $glo_pwd/results/thermalization/tequilYESlist.txt | awk '{print $1}') 
echo "$ttraj out of $glo_ntraj trajectories are valid for TPMD analysis"  >> $glo_pwd/results/log.txt	

if [ $ttraj -ne $glo_ntraj ]
then
echo "The list of discarded trajecotries in this step can be found in results/thermalization/tequilDISCARDlist.txt file" >> $glo_pwd/results/log.txt
fi

echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt
echo "" >> $glo_pwd/results/log.txt

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Begin parallel transition detection processs~~~~~~~~~~~~~~~~~~~~

cd $glo_pwd
cd $glo_pwd/src_tools/fpt_analysis_files

chmod +x *.sh
chmod +x *.awk

	if [ -e $glo_pwd/results/thermalization/tequilYESlist.txt ]
	then
	echo "~~~~~~~~~~PARALLELIZATION~~~~~~~~~~" >> $glo_pwd/results/log.txt
	echo "" >> $glo_pwd/results/log.txt
	$glo_pwd/src_tools/fpt_analysis_files/parallel.sh &>> $glo_pwd/results/error.txt &
	else
	echo "No trajectory is valid to be scanned for TPMD after thermalization check"
	fi 
