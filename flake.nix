{
  description = "A Rust EXI codec";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
    naersk.url = "github:nix-community/naersk";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    naersk,
    devshell,
    fenix,
  }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [fenix.overlays.default];
      };
      rust = (pkgs.fenix.stable.withComponents [
        "cargo"
        "clippy"
        "rust-src"
        "rustc"
        "rustfmt"
      ]);
      # Override naersk to use our chosen rust version from rust-overlay
      naersk-lib = naersk.lib.${system}.override {
        cargo = rust;
        rustc = rust;
      };
    in rec {
      packages.default = naersk-lib.buildPackage {
        pname = "exi";
        root = ./.;
      };

      apps.default = utils.lib.mkApp {drv = packages.default;};

      # Provide a dev env with rust and rust-analyzer
      devShells.default = let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [fenix.overlays.default devshell.overlays.default];
        };
      in
        pkgs.devshell.mkShell {
          packages = with pkgs; [rust rust-analyzer];
        };
      formatter = pkgs.alejandra;
    });
}
