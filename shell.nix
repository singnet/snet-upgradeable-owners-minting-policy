let
  plutus_apps = builtins.getFlake "github:IntersectMBO/plutus-apps?ref=v1.1.0";
in
import "${plutus_apps}/shell.nix"
