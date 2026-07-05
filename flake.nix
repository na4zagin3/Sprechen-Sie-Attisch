{
  description = "Sprechen Sie Attisch? document build";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          converter = pkgs.haskellPackages.mkDerivation {
            pname = "convert-multilingual-doc";
            version = "0.1.0.0";
            src = ./convert-multilingual-doc;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = with pkgs.haskellPackages; [
              aeson
              base
              bytestring
              containers
              yaml
            ];
            license = pkgs.lib.licenses.bsd3;
          };
          fonts = with pkgs; [
            libertine
            oldstandard
            source-code-pro
            source-sans-pro
            source-serif-pro
            theano
          ];
          tex = pkgs.texlive.combine {
            inherit (pkgs.texlive)
              collection-fontsrecommended
              collection-langgerman
              collection-langgreek
              collection-langjapanese
              collection-latexextra
              collection-luatex
              latexmk
              scheme-small;
          };
        in {
          inherit converter;
          default = pkgs.stdenvNoCC.mkDerivation {
            pname = "sprechen-sie-attisch";
            version = "1.0";
            src = pkgs.lib.cleanSource ./.;

            nativeBuildInputs = [ converter pkgs.fontconfig tex ];
            FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = fonts; };
            OSFONTDIR = pkgs.lib.concatMapStringsSep ":"
              (font: "${font}/share/fonts//") fonts;
            LANG = "C.UTF-8";
            LC_ALL = "C.UTF-8";

            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR"
              luaotfload-tool --update --force
              latexmk -C SprechenSieAttisch-ja-1.tex
              latexmk -C SprechenSieAttisch-ja-2.tex
              latexmk -C SprechenSieAttisch-ja.tex
              ${pkgs.bash}/bin/bash ./build.sh
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p "$out"
              cp SprechenSieAttisch-ja-1.pdf "$out/"
              cp SprechenSieAttisch-ja-2.pdf "$out/"
              cp SprechenSieAttisch-ja.pdf "$out/"
              runHook postInstall
            '';
          };
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          project = self.packages.${system}.default;
        in {
          default = pkgs.mkShell {
            inputsFrom = [ project ];
            shellHook = ''
              export FONTCONFIG_FILE="${project.FONTCONFIG_FILE}"
              export OSFONTDIR="${project.OSFONTDIR}"
              export LANG=C.UTF-8
              export LC_ALL=C.UTF-8
            '';
          };
        });
    };
}
