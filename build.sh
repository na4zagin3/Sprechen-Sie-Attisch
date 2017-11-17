#!/bin/sh

function convert_part () {
  stack exec -- convert-multilingual-doc < "../part-$1.yaml" > "../part-$1.tex"
}

pushd convert-multilingual-doc
stack build
convert_part a
convert_part b
convert_part c
popd

latexmk -xelatex SprechenSieAttisch.tex
