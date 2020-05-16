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
#Script name: neb.sh
#
#Purpose: As per the input from user, it generates input file for LAMMPS NEB calculations for the atoms and final frame chosen by user.
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 11 May 2020
#.......................................................................................................................................

cd $glo_pwd/src_tools/neb
atomdif=$( expr $atom2 - $atom1 + 1 )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~List down the displacement of an atom from its initial position in file

if [ ! -d "NEB" ]
then
mkdir $glo_pwd/src_tools/neb/NEB
fi



##VMD

blocksize=$(($glo_natom+9)) #BLOCK SIZE in lammps output trajectory

frame=$frametime

nebfframe=$( expr $blocksize \* $frame + $atom1 )
neblframe=$( expr $blocksize \* $frame + $atom2 + 9 )
#echo $neblframe $nebfframe $glo_trajfile
sed -n "$nebfframe,$neblframe p" $glo_trajfile > tmp.neb
sed -i -e 1,9d tmp.neb
echo $atomdif > NEB/neb.final
awk '{print $1" "$3" "$4" "$5}' tmp.neb >> NEB/neb.final  #TRAJECTORY XYZ Cordinate columns
rm tmp.neb $glo_trajfile

cp -r $glo_pwd/src_tools/neb/NEB $glo_pwd/results/NEBinput

		if [ -d "$glo_pwd/results/NEBinput/$foldername" ]
		then
		rm -r $glo_pwd/results/NEBinput/$foldername
		fi
	mv $glo_pwd/results/NEBinput/NEB $glo_pwd/results/NEBinput/$foldername
	mv $glo_pwd/results/NEBinput/$foldername/neb.final  $glo_pwd/results/NEBinput/$foldername/$filename.neb.txt


cd $glo_pwd
