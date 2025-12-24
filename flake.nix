{
  description = "A Nix-flake-based Node.js development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs = { self , nixpkgs ,... }: let
    # system should match the system you are running on
    system = "x86_64-linux";
  in {
    devShells."${system}".default = let
      pkgs = import nixpkgs {
        inherit system;
      };
    in pkgs.mkShell {
      # create an environment with nodejs_18, pnpm, and yarn
      packages = with pkgs; [
        nodejs
	nodePackages.pnpm
        elmPackages.elm
        elmPackages.elm-format
        elmPackages.elm-language-server
      ];

      shellHook = ''
        echo "node `${pkgs.nodejs}/bin/node --version`"
      '';
    };
  };
}
