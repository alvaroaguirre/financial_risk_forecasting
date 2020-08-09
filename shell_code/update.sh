#!/bin/bash
for d in Seminar*; do
    cd "$d"
    jupyter nbconvert *.ipynb --to script 
    jupyter nbconvert *.ipynb --to html
    jupyter nbconvert *.ipynb --to PDFviaHTML
    cd ..
done

cd R_for_FM442
jupyter nbconvert *.ipynb --to script
jupyter nbconvert *.ipynb --to html
jupyter nbconvert *.ipynb --to PDFviaHTML
cd ..

cd weekly_classwork
pandoc -o weekly_classwork.pdf weekly_classwork.md
cd ..