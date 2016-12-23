#!/bin/bash

FILES=/projektit/finunipolicy/data/*.zip

for FILE in $FILES; do
  echo "Processing $FILE..."
  outfile=$(echo $FILE | cut -d "/" -f 9 | cut -d "." -f 1)
  java -jar ~/saxonee/saxon9ee.jar $FILE parse.xsl -o:data/$outfile.csv
done



