#!/bin/bash

for i in {1..1000} #to create trajectories numbered 1 to 1000, you can change these numbers as per your requirement.
do
mkdir $i
cp submit.sh $i
cp in.lfp $i
cp ag.txt $i
cp Ag_u3.eam $i
cp random.txt $i
num=$( sed -n " $i p " random.txt  | awk '{ print $1 }')
cd $i
sed -i -e 's/seed/'$num'/g' in.lfp
qsub submit.sh
cd ..
done

