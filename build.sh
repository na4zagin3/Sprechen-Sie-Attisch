#!/bin/sh
pushd convert-multilingual-doc
stack build
stack exec -- convert-multilingual-doc < ../part-a.yaml > ../part-a.tex
popd

latexmk -xelatex SprechenSieAttisch.tex
