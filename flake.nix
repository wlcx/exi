{
  description = "A Rust EXI codec";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    w3c-testsuite = {
      url = "github:w3c/exi-testsuite";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    utils,
    crane,
    fenix,
    w3c-testsuite,
  }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [fenix.overlays.default];
      };
      pkgs-unstable = import nixpkgs-unstable {
        inherit system;
      };
      rust = (pkgs.fenix.stable.withComponents [
        "cargo"
        "clippy"
        "rust-src"
        "rustc"
        "rustfmt"
      ]);
      craneLib = (crane.mkLib pkgs).overrideToolchain (_: rust);
      src = craneLib.cleanCargoSource ./.;
      commonArgs = {
        inherit src;
        strictDeps = true;
      };
      cargoArtifacts = craneLib.buildDepsOnly commonArgs;
      individualCrateArgs = commonArgs // {
        inherit cargoArtifacts;
        inherit (craneLib.crateNameFromCargoToml { inherit src; }) version;
        # NB: we disable tests since we'll run them all via cargo-nextest
        doCheck = false;
      };
      fileSetForCrate = crate: pkgs.lib.fileset.toSource {
        root = ./.;
        fileset = pkgs.lib.fileset.unions [
          ./Cargo.toml
          ./Cargo.lock
          ./src
          crate
        ];
      };
      exicmd = craneLib.buildPackage (individualCrateArgs // {
        pname = "exicmd";
        cargoExtraArgs = "-p exicmd";
        src = fileSetForCrate ./exicmd;
      });
      testData = "${w3c-testsuite}/data/interop";
    in rec {
      packages.default = exicmd;

      apps.default = utils.lib.mkApp {drv = packages.default;};

      # Provide a dev env with rust and rust-analyzer
      devShells.default = craneLib.devShell {
        #checks = self.checks.${system};
        packages = [ pkgs.rust-analyzer ];
      };
      formatter = pkgs.alejandra;

      checks.interop = (pkgs.runCommand "interop_tests" {
        src = ./.;
        nativeBuildInputs = [
          packages.default
          pkgs-unstable.exificient
          pkgs.oils-for-unix
          pkgs.difftastic
          # For xmllint
          pkgs.libxml2
        ];
      } ''
        $src/test/interop.ysh ${testData}/builtInGrammar
      '');
    });
}
