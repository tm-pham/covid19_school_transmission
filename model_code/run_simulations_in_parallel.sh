#!/bin/bash
#test/simulations/
echo "Run file to combine simulations."
results_folder="/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/results/"
prefix=("covid19school_simulation")
delim=("_")
suffix=(".R")

simulation_folder=("simJ1")
file_name=("simJ1")

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

# simulation_folder=("simB1")
# file_name=("simB1")
#
# cd /Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/
# mkdir $results_folder$simulation_folder
#
# for i in {5..6}
# do
#   (sed -ie "s/folder='.*'/folder='$simulation_folder'/g" covid19school_simulation30.R
#   echo $prefix$delim$file_name$delim$i$suffix
#   cp covid19school_simulation30.R  "$prefix$delim$file_name$delim$i$suffix"
#   sed -ie "s/save.image(file=paste0(resultsPath,folder,'.*'))/save.image(file=paste0(resultsPath,folder,'\/$file_name$delim$i.RData'))/g" $prefix$delim$file_name$delim$i$suffix
#   sed -ie "s/for(scenario in .*)/for(scenario in $i)/g" $prefix$delim$file_name$delim$i$suffix
#   Rscript $prefix$delim$file_name$delim$i$suffix) &
# done
# wait
#
# simulation_folder=("simH1")
# file_name=("simH1")
#
# cd /Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/
# mkdir $results_folder$simulation_folder
#
# for i in {5..6}
# do
#   (sed -ie "s/folder='.*'/folder='$simulation_folder'/g" covid19school_simulation75.R
#   echo $prefix$delim$file_name$delim$i$suffix
#   cp covid19school_simulation75.R  "$prefix$delim$file_name$delim$i$suffix"
#   sed -ie "s/save.image(file=paste0(resultsPath,folder,'.*'))/save.image(file=paste0(resultsPath,folder,'\/$file_name$delim$i.RData'))/g" $prefix$delim$file_name$delim$i$suffix
#   sed -ie "s/for(scenario in .*)/for(scenario in $i)/g" $prefix$delim$file_name$delim$i$suffix
#   Rscript $prefix$delim$file_name$delim$i$suffix) &
# done
# wait
