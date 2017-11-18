#!/bin/sh

function convert_part () {
  echo "Converting Part $1"
  stack exec -- convert-multilingual-doc --part-to-file "../part-$1.tex" "../part-$1.yaml" || exit 1
}

pushd convert-multilingual-doc
stack build
convert_part a
convert_part b
convert_part c
popd

latexmk -xelatex SprechenSieAttisch.tex
