#!/bin/bash
runKnitr.sh R-basic-stats.Rnw

# I am not sure the html makes much sense 
# and it is broken somewhere now
# ./make-html.sh R-basic-stats.Rnw

zip -r Additional_files_R-basic-stats.zip\
 R-basic-stats.R P53.txt MYC.txt BRCA2.txt\
 MIT.txt Cholesterol.txt AnAge_birds_reptiles.txt\
 CystFibr2.txt
   
