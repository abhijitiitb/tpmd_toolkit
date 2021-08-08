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
#Script name: fpt.sh
#
#Purpose: Sorts 'transition list' and checks for corelated atomic displacements in transition. Creates mechanism file which can be #further used for state indexing and state vs FPT data collection; mentioning information like FPT, atomic indexes and coordinates.
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com)
#
#Date of Modification: 13 August 2019
#.......................................................................................................................................

cat tmp.trans > trans.all
sort -k1 -n trans.all -o trans.all 
nlin=$(wc -l trans.all | awk '{print $1}') 

for ((i=1;i<=$nlin;i++))
do
  mark=$( sed -n " $i p " trans.all  | awk '{ print $1}') 
  m1=$(echo $mark | awk '{print $1}')

  j=$( expr $i + 1 )
  bark=$( sed -n " $j p " trans.all  | awk '{ print $1}')
  b1=$(echo $bark | awk '{print $1}')
	
	if [ -z $b1 ]
	then
	b1=$( expr $glo_nfpt + $m1 + 1 )
	fi

  framedif=$( expr $b1 - $m1 )


	if [ $framedif -gt $glo_nfpt ] 
	then
   	  stopline=$i
	  break
	fi
done

if [ -z "$stopline" ] 
then
  exit 
fi

startline=1

sed -n -e "$startline,$stopline p" trans.all > trans.sort 

nlin2=$(wc -l trans.sort | awk '{print $1}') 

for((n=1;n<=$nlin2;n++))
do
checktype=$( sed -n "$n p" trans.sort  | awk '{ print $3}')
	if [ $checktype -eq 2 ]
	then
	exit
	fi
done

tframe=$(head -1 trans.sort | awk '{print $1}') 
cframe=$(tail -1 trans.sort | awk '{print $1}') 
pframe=$( expr $cframe - 1 )
pfptblock=$( expr $pframe \* $glo_blocksize )
lclv_fpt=$( echo "$tframe*$glo_time" | bc) 
export lclv_fpt
sort -k2 -n trans.sort -o trans.sort 
for((k=1;k<=$nlin2;k++))
do
	iatom=$( sed -n "$k p" trans.sort  | awk '{ print $2}')
	frame=$( sed -n "$k p" trans.sort  | awk '{ print $1}')
	line=$( expr $pfptblock + $iatom + 9 ) 

	txt1=$( sed -n " $line p " $glo_pwd/trajectories/$lclv_itraj/$glo_trajfile | awk '{print $3" "$4" "$5}')
	x1=$(echo $txt1 | awk '{print $1}')
	y1=$(echo $txt1 | awk '{print $2}')
	z1=$(echo $txt1 | awk '{print $3}')
	echo $iatom $frame $x1 $y1 $z1" "#atom_number fpt_frame x y z"  "#x y z coordinates of stablemost frame  >> trans.fpt
done

for((k=1;k<=$nlin2;k++))
do
	shark=$( sed -n "$k p" trans.fpt  | awk '{ print $1}')
	echo -n $shark" ">> trans.fpt
done
sed '1h;1d;$!H;$!d;G' trans.fpt > trans.fpttemp 
mv trans.fpttemp trans.fpt


sed -i '1i'$nlin2' :number of atoms did transitoin from their starting position during FPT \' trans.fpt 

echo "$cframe : frame to be captured for all atoms in final state" >> trans.fpt
echo "$lclv_fpt :first passage time for this trajectory" >> trans.fpt


