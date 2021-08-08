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
#Script name: dispVSframe.sh
#
#Purpose: Plots displacement of atoms from initial position in every frame of trajectory for the choice of atoms asked by user.
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 11 May 2020
#.......................................................................................................................................


cp $glo_pwd/trajectories/$traj/$glo_trajfile $glo_pwd/src_tools/dispVSframe/

cd $glo_pwd/src_tools/dispVSframe

	if [ ! -d "process" ]
	then
	mkdir $glo_pwd/src_tools/dispVSframe/process
	fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~List down the displacement of an atom from its initial position in file
function_displacement(){

txt=$( sed -n "1 p" tmp.2 | awk '{print $1" "$2" "$3}')
x0=$(echo $txt | awk '{print $1}')
y0=$(echo $txt | awk '{print $2}')
z0=$(echo $txt | awk '{print $3}')

awk -v x0=$x0 -v y0=$y0 -v z0=$z0 \
'{sqdisp=($1-x0)*($1-x0)+($2-y0)*($2-y0)+($3-z0)*($3-z0); printf "%4f \n", sqdisp}' tmp.2 \
> sqdisp.txt			

if [ -e tmp.txt. ]
then
rm tmp.txt.			
fi

for((ll=1;ll<=$glo_nframe;ll++))
do
  sqdispcopy=$( sed -n "$ll p" sqdisp.txt  | awk '{ print $1}')
  disp=$(echo "sqrt($sqdispcopy)" | bc)
  echo $disp" "$ll >>tmp.txt.
done


cat tmp.txt. > ./process/$lclv_iatom.displacement.dat


}


for ((lclv_iatom=$atom1; lclv_iatom<=$atom2; lclv_iatom++))
do

#~~~~~~~~~~~~~~ Extracting atom's position coordinates in each consequtive frames from trajectory file


startline=$(($lclv_iatom+9)) #line number in block where atom appears in lammps output trajectory

blocksize=$(($glo_natom+9)) #block size in lammps output trajectory

#print line belonging to lclv_iatom
sed -n "${startline}~${blocksize}p" $glo_pwd/src_tools/dispVSframe/$glo_trajfile > tmp.0

#save coordinates to file
awk '{print $3" "$4" "$5}' tmp.0 > tmp.1

#taking care of PBC( Periodic Boundary Condtions)
$glo_pwd/src_tools/dispVSframe/unpbc.sh tmp.1 $glo_pbcx $glo_pbcy $glo_pbcz tmp.2


function_displacement

done

cd $glo_pwd/src_tools/dispVSframe/process
echo > gnuplot.in
echo "set xlabel \"frame\"" >> gnuplot.in
echo "set ylabel \"displacement(A)\"" >> gnuplot.in
echo "set term pdf" >> gnuplot.in
echo "set output \"dispVSframe.pdf\"" >> gnuplot.in
echo plot for [i=$atom1:$atom2] \'\'.i.\'.displacement.dat\' u 2:1 with line notitle >> gnuplot.in
gnuplot gnuplot.in

rm *.displacement.dat
rm gnuplot.in
cd $glo_pwd/src_tools/dispVSframe
rm tmp.* sqdisp.* $glo_trajfile

cp $glo_pwd/src_tools/dispVSframe/process/dispVSframe.pdf $glo_pwd/results/plots
mv $glo_pwd/results/plots/dispVSframe.pdf $glo_pwd/results/plots/$filename


cd $glo_pwd
