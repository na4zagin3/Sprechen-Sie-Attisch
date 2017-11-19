#!/bin/sh

function convert_part () {
  echo "Converting Part $1"
  stack exec -- convert-multilingual-doc --part-to-file "../part-$1.tex" "../part-$1.yaml" || exit 1
}

function convert_lexicon () {
  echo "Converting Lexicon"
  stack exec -- convert-multilingual-doc --lexicon-to-file "../lexicon.tex" "../lexicon.yaml" || exit 1
}

pushd convert-multilingual-doc
stack build
convert_part a
convert_part b
convert_part c
convert_part d
convert_part e
convert_part f
convert_part g
convert_part h
convert_part i
convert_lexicon
popd

latexmk -xelatex SprechenSieAttisch.tex
