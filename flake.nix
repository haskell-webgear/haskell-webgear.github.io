{
  description = "WebGear Documentation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        python = pkgs.python3.withPackages (ps: [
          ps.mike
          ps.mkdocs-material
        ]);
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.mkdocs
            python
          ];
        };
      });
}
