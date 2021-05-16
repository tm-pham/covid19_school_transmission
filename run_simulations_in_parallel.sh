#!/bin/bash
#test/simulations/
echo "Run file to combine simulations."
results_folder="/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/results/"
simulation_folder=("simD")
file_name=("simD")
prefix=("covid19school_simulation")
delim=("_")
suffix=(".R")

cd /Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/
mkdir $results_folder$simulation_folder


for i in {1..6}
do
  (sed -ie "s/folder='.*'/folder='$simulation_folder'/g" covid19school_simulation.R
  echo $prefix$delim$file_name$delim$i$suffix
  cp covid19school_simulation.R  "$prefix$delim$file_name$delim$i$suffix"
  sed -ie "s/save.image(file=paste0(resultsPath,folder,'.*'))/save.image(file=paste0(resultsPath,folder,'\/$file_name$delim$i.RData'))/g" $prefix$delim$file_name$delim$i$suffix
  sed -ie "s/for(scenario in .*)/for(scenario in $i)/g" $prefix$delim$file_name$delim$i$suffix
  Rscript $prefix$delim$file_name$delim$i$suffix) &
done
wait

# simulation_folder=("simE")
# file_name=("simE")
#
# cd /Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/
# mkdir $results_folder$simulation_folder
#
# for i in {1..6}
# do
#   (sed -ie "s/folder='.*'/folder='$simulation_folder'/g" covid19school_simulation30.R
#   echo $prefix$delim$file_name$delim$i$suffix
#   cp covid19school_simulation30.R  "$prefix$delim$file_name$delim$i$suffix"
#   sed -ie "s/save.image(file=paste0(resultsPath,folder,'.*'))/save.image(file=paste0(resultsPath,folder,'\/$file_name$delim$i.RData'))/g" $prefix$delim$file_name$delim$i$suffix
#   sed -ie "s/for(scenario in .*)/for(scenario in $i)/g" $prefix$delim$file_name$delim$i$suffix
#   Rscript $prefix$delim$file_name$delim$i$suffix) &
# done
# wait
