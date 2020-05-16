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
#Script name: plot.sh
#
#Purpose: Plots two important graphs. 1) Counts of occurnces of states in scan 2) Distribution of FPTs obtained from all trajectories
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in), Saurabh Shivpuje (saurabh.shivpuje@gmail.com), Manish Kumawat
#
#Date of Modification: 13 August 2019
#.......................................................................................................................................

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Generate states v/s count of their occurances~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
awk ' NR>1 {print $1}' $glo_pwd/results/statestat.txt | sort | uniq -c > $glo_pwd/results/plots/statecount.txt
awk '{print $2" "$1}' $glo_pwd/results/plots/statecount.txt > $glo_pwd/results/plots/statecount2.txt
sort -k1 -n $glo_pwd/results/plots/statecount2.txt -o $glo_pwd/results/plots/statecount.txt 
rm $glo_pwd/results/plots/statecount2.txt

cd $glo_pwd/results/plots

echo > statecount.in
echo "set style data histogram" >> statecount.in
echo "set lmargin at screen 0.20" >> statecount.in
echo "set rmargin at screen 0.55" >> statecount.in
echo "set bmargin at screen 0.25" >> statecount.in
echo "set tmargin at screen 0.85" >> statecount.in
echo "set tics font 'Courier,20'">> statecount.in
echo "set border 15 back lw 3" >> statecount.in
echo "set xtics nomirror" >> statecount.in
echo "set ytics nomirror" >> statecount.in
echo "set xtics rotate by -45" >> statecount.in
echo "set xlabel font 'Arial,24'" >> statecount.in
echo "set ylabel font 'Arial,24'" >> statecount.in
echo "set xtics offset 0.5" >> statecount.in
echo "set yrange [0:]" >> statecount.in
echo "set xlabel 'State index' offset 0,-1" >> statecount.in
echo "set ylabel 'Count' offset -5,0" >> statecount.in
echo "set style fill solid border 0" >> statecount.in
echo "set boxwidth 2.5" >> statecount.in
echo "set style fill solid" >> statecount.in
echo "set term pdf" >> statecount.in
echo "set output \"statecount.pdf\"" >> statecount.in
echo "plot 'statecount.txt' using 2:xtic(1) linecolor 'green' notitle"  >> statecount.in
gnuplot statecount.in

rm statecount.in


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Generate fpt v/s count of their occurances in range~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
awk ' NR>1 {print $2}' $glo_pwd/results/statestat.txt > $glo_pwd/results/plots/fptcount.txt
sort -k1 -n $glo_pwd/results/plots/fptcount.txt -o $glo_pwd/results/plots/fptcount2.txt
rm $glo_pwd/results/plots/fptcount.txt

for((i=0;i<$glo_tempstep;i++))
do
a=$(echo " $i * $glo_stepperiod + 1 " |bc )
ii=$( expr $i + 1 )
b=$(echo " $ii * $glo_stepperiod " |bc )
count=$(cat $glo_pwd/results/plots/fptcount2.txt| awk '{if ( ( $1>='$a') && ($1<'$b')) {print 1};}' | awk '{sum+=$1} END {if (sum == 0) {print 0} else {print sum}}')
echo $a-$b" "$count >> fptcount.txt
done

echo > fptcount.in
echo "set style data histogram" >> fptcount.in
echo "set lmargin at screen 0.20" >> fptcount.in
echo "set rmargin at screen 0.55" >> fptcount.in
echo "set bmargin at screen 0.25" >> fptcount.in
echo "set tmargin at screen 0.85" >> fptcount.in
echo "set tics font 'Courier,20'">> fptcount.in
echo "set border 15 back lw 3" >> fptcount.in
echo "set xtics nomirror" >> fptcount.in
echo "set ytics nomirror" >> fptcount.in
echo "set xtics rotate by -45" >> fptcount.in
echo "set xlabel font 'Arial,24'" >> fptcount.in
echo "set ylabel font 'Arial,24'" >> fptcount.in
echo "set xtics offset 0.5" >> fptcount.in
echo "set yrange [0:]" >> fptcount.in
echo "set xlabel 'State index' offset 0,-1" >> fptcount.in
echo "set ylabel 'Count' offset -5,0" >> fptcount.in
echo "set style fill solid border 0" >> fptcount.in
echo "set boxwidth 2.5" >> fptcount.in
echo "set style fill solid" >> fptcount.in
echo "set term pdf" >> fptcount.in
echo "set output \"fptcount.pdf\"" >> fptcount.in
echo "plot 'fptcount.txt' using 2:xtic(1) linecolor 'green' notitle"  >> fptcount.in
gnuplot fptcount.in

rm fptcount.in
rm fptcount2.txt

 
