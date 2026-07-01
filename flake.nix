{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      
      packages.x86_64-linux.default = pkgs.haskellPackages.developPackage {
        root = ./.;
        modifier = drv: pkgs.haskell.lib.addExtraLibraries drv (with pkgs.haskellPackages; [ 
          pkgs.haskell-language-server
        ]);
      };
      
    };
}
