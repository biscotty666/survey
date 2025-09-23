{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.05";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        #pkgs = nixpkgs.legacyPackages.${system};
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      myvscode = pkgs.vscode-with-extensions.override {
        vscodeExtensions = (with pkgs.vscode-extensions; [
          enkia.tokyo-night
          sainnhe.gruvbox-material
          vscodevim.vim
          reditorsupport.r
        ]);};
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            R
            quarto
            chromium
            pandoc
            texlive.combined.scheme-full
            rstudio
            radianWrapper
            grass
            inkscape
            myvscode
            (with rPackages; [
              DiagrammeR
              Matrix
              bookdown
              broom
              censusapi
              ggpattern
              gt
              gtsummary
              haven
              hexbin
              janitor
              labelled
              naniar
              osfr
              pagedown
              prettyunits
              quarto #
              rnaturalearth
              rnaturalearthdata
              sf
              srvyr
              styler
              survey
              survival
              tidycensus
              tidyselect
              tidyverse #
            ])
          ];
          shellHook = ''
          '';
        };
      }
    );
}
