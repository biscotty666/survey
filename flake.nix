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
      
      srvyrexploR = pkgs.rPackages.buildRPackage {
        name = "srvyrexploR";
        src = pkgs.fetchFromGitHub{
          owner = "tidy-survey-r";
          repo = "srvyrexploR";
          rev = "7f3221451b0f7933dee23ac9167ee84d60e4c6d2";
          sha256 = "05qlx8mifqd03qmar4a7lh3gqwywjkxsf59i40s29xzdxxyspkqw";
        };
   propagatedBuildInputs = with pkgs.rPackages; [bslib evaluate jsonlite knitr stringr tinytex yaml xfun];
      };
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
            (with rPackages; [
              DiagrammeR
              Matrix
              bookdown
              broom
              cardx
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
              srvyrexploR
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
