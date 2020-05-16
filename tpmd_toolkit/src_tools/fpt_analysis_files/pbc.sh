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
#Script name: pbc.sh
#
#Purpose: Undo the effect of periodic boundary conditions. 
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
#
#Date of Modification: 13 August 2019
#.......................................................................................................................................

#Perform initial checks
if [ $#  -eq 0 ]; then
   echo Provide input file and BoxSize ...
   echo ... usage: unpbc filein BoxX BoxY BoxZ fileout OR
   echo ..         unpbc filein Box fileout
   exit
fi
   
if [ $#  -eq 3 ]; then #unpbc filein BoxX fileout
   filein=$1
   BoxX=$2
   BoxY=$2
   BoxZ=$2
   fileout=$3
elif [ $#  -eq 5 ]; then #unpbc filein BoxX BoxY BoxZ fileout
   filein=$1
   BoxX=$2 ##27.14157600
   BoxY=$3
   BoxZ=$4
   fileout=$5
else
   echo Provide input file and BoxSize ...
   echo ... usage: unpbc filein BoxX BoxY BoxZ fileout OR
   echo .......... unpbc filein Box fileout
   exit
fi

if [ -f $filein ]; then
   Nframes=$(wc -l $filein | awk '{print $1}')
   #echo Analyzing $Nframes lines in $filein with BoxSize [$BoxX,$BoxY,$BoxZ]
else
   echo Error: $filein not found
   exit
fi

#read number of columns/fields
Nfields=$(head -1 $filein | awk '{print NF}')
if [ $Nfields != "3" ]; then
   echo Error: Incorrect file format - Number of fields: $Nfields
   echo     Expecting file of the type
   echo     0 0 0
   echo     -0.0626342 0.103466 -0.532503
   echo     -0.308072 -0.146489 0.244263 ...
   exit
fi

#create a copy with last row missing
Nlines=$(($Nframes-1))
sed -n "1,${Nlines}p" $filein > tmp445646
 
#create a copy with one row shifted
sed -n '2,$p' $filein > tmp445647

#stitch columns side by side
paste tmp445646 tmp445647 > tmp445648

#find the difference between columns and print the difference
#this difference is nothing but the displacement with respect
#to the previous frame
awk '{dx=$4-$1; dy=$5-$2; dz=$6-$3; print dx" "dy" "dz}' tmp445648 > tmp445649

#remove effect of boundary condition from the displacement
cat tmp445649 | awk -v BoxX=$BoxX -v BoxY=$BoxY -v BoxZ=$BoxZ -f $lclv_pwd/sign.awk > tmp445650

#add displacement to original coordinate and save to fileout
txt=$(head -1 $filein)
x0=$(echo $txt | awk '{print $1}')
y0=$(echo $txt | awk '{print $2}')
z0=$(echo $txt | awk '{print $3}')
head -1 $filein > $fileout
awk -v x0=$x0 -v y0=$y0 -v z0=$z0 '{print $1+x0" "$2+y0" "$3+z0; x0=x0+$1; y0=y0+$2; z0=z0+$3}' tmp445650 >> $fileout

#remove tmp file
rm tmp445646 tmp445647 tmp445648 tmp445649 tmp445650

