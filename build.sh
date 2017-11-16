#!/bin/sh
pushd convert-multilingual-doc
stack build
stack exec -- convert-multilingual-doc < ../part-a.yaml > ../part-a.tex
stack exec -- convert-multilingual-doc < ../part-b.yaml > ../part-b.tex
popd

latexmk -xelatex SprechenSieAttisch.tex
