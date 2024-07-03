{ pkgs ? import <nixpkgs> { } }:
let
  plutus_apps = builtins.getFlake "github:IntersectMBO/plutus-apps?ref=v1.1.0";
  # nix_lib = builtins.getFlake "git+https://github.com/zmrocze/nix-lib";
in
(pkgs.mkShell {
  packages = [ pkgs.nodejs_20 ];

  inputsFrom = [ (import "${plutus_apps}/shell.nix" { }) ];

  shellHook = ''
    echo "node --version: " $(node --version)
    echo "npm --version: " $(npm --version)
    echo "Now install tsc typescript compiler. I think it's:
    echo "$ npm i --save-dev typescript"
    echo "$ npm i tsc"
    echo "Then in ./tests try:"
    echo "$ npm exec -c tsc && node dist/tests/offchain_emulated.js"
    echo "to execute the offchain_emulated.js test."
    echo ""
    echo "Regenerate scripts with `cabal run`"
  '';
})
