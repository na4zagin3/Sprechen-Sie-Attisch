#!/usr/bin/env bash

function convert_part () {
  echo "Converting Part $1"
  "${CONVERTER[@]}" --part-to-file "../part-$1.tex" "../part-$1.yaml" || exit 1
}

function convert_lexicon () {
  echo "Converting Lexicon"
  "${CONVERTER[@]}" --lexicon-to-file "../lexicon.tex" "../lexicon.yaml" || exit 1
}

pushd convert-multilingual-doc
if command -v convert-multilingual-doc >/dev/null 2>&1; then
  CONVERTER=(convert-multilingual-doc)
else
  command -v stack >/dev/null 2>&1 || {
    echo "Neither convert-multilingual-doc nor stack is available." >&2
    echo "Run 'nix develop' to enter the project environment." >&2
    exit 1
  }
  stack build
  CONVERTER=(stack exec -- convert-multilingual-doc)
fi
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

# latexmk -xelatex -interaction=nonstopmode SprechenSieAttisch-Fraktur.tex
# latexmk -g -interaction=nonstopmode -xelatex SprechenSieAttisch-ja-2.tex

# latexmk -xelatex SprechenSieAttisch.tex
# latexmk -xelatex -interaction=nonstopmode SprechenSieAttisch-Fraktur.tex
# latexmk -lualatex -interaction=nonstopmode SprechenSieAttisch-Antiqua.tex
# latexmk -lualatex -interaction=nonstopmode SprechenSieAttisch-ja-2.tex

# latexmk -g -lualatex -interaction=nonstopmode SprechenSieAttisch-ja-1.tex
# latexmk -g -xelatex -jobname=SprechenSieAttisch-ja-xe -interaction=nonstopmode SprechenSieAttisch-ja.tex
latexmk -g -lualatex SprechenSieAttisch-ja-3.tex
latexmk -g -xelatex -interaction=nonstopmode -jobname=SprechenSieAttisch-ja-3-xe SprechenSieAttisch-ja-3.tex
