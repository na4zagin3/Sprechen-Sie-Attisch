# Sprechen Sie attisch?
Digitalized "Sprechen Sie attisch?" with LaTeX.

E. Joannides: _Sprechen Sie Attisch? Moderne Conversation in altgriechischer Umgangssprache nach den besten attischen Autoren._ Koch, Leipzig 1889, <https://archive.org/details/sprechensieatti00johngoog>.

## Notes

**All the footnotes are given by me. The original book has none of them.**

I added additional material “Redaktionalle Hinweise zur Digitaliserung und Setzung des Buches” at the end.

I appreciate any comments, especially corrections of typographical and grammatical errors.

## Building with Nix

Build the generated TeX and all three Japanese PDFs reproducibly:

```console
nix build
```

The PDFs are written to `result/`. For an interactive environment containing
the converter, LuaLaTeX, and the required fonts, run:

```console
nix develop
./build.sh
```

If flakes are not enabled globally, add
`--extra-experimental-features 'nix-command flakes'` immediately after `nix`
in those commands.

## Dependency

- Nix (recommended), or Stack plus a LuaLaTeX installation and the fonts used
  in `preamble.tex`

## License

All files under the project are licensed under GPL version 2, 3 or later.
