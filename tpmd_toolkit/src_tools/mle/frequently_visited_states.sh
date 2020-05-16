#finds the maximum state index in the file (filein) contaning first passage times from TPMD
#Usage: ./find_max_index.sh filein

#Perform initial checks
if [ $#  -eq 0 ]; then
   echo Provide input filename ...
   echo ... usage: ./find_max_index.sh filein
   echo ..     where filein contains the first passage times
   echo ..     collected from TPMD calculations
   exit
fi
filename=$1
awk -v maxindex=0 '{if($1>maxindex){maxindex=$1}}END{print maxindex " number of states found"; print NR-1 " number of first passage times"}' $filename

maxindex=$(awk -v maxindex=0 '{if($1>maxindex){maxindex=$1}}END{print maxindex}' $filename)

touch tmp.2t824b23f
rm tmp.2t824b23f
echo ""
echo printing number of transitions for states
echo State NumberTransitions
for istate in `seq 1 $maxindex`
do
n=$(awk -v is=$istate '{if($1==is){c=c+1}}END{print c}' $filename)
echo $istate $n >> tmp.2t824b23f
done
sort -k2 -n -r tmp.2t824b23f
rm tmp.2t824b23f
