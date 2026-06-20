{
  description = "dev env";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gleam
            erlang_28
            rebar3
            bun
            just
            mailpit
          ];

          shellHook = ''
            echo "dev env loaded"
            just --list
            echo "Use just to run them."
          '';
        };
      }
    );
}
