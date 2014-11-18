#!/bin/bash
runKnitr.sh R-basic-stats.Rnw
rm figure/*.pdf
rm figure/*.png
./make-html.sh R-basic-stats.Rnw
zip -r Additional_files_R-basic-stats.zip\
 R-basic-stats.R P53.txt MYC.txt BRCA2.txt\
 MIT.txt Cholesterol.txt AnAge_birds_reptiles.txt\
 CystFibr2.txt
   
