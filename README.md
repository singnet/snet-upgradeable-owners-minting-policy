# snet-upgradeable-owners-minting-policy


## Instructions 

Built on top of plutus-apps v1.1.0, `cabal.project` refers to its packages. Make sure to install `plutus-apps` first.

### Compile & interact with scripts
At the root directory enter `nix shell`:
```sh
    nix-shell
```

Then execute `cabal run create-token` with needed arguments.
There are four arguments:
 - transaction hash, used to identify the specific transaction outputs
 - id of specific transaction output to spend
 - token name
 - path or folder under `scripts` folder in the root directory, where new Plutus Scripts for token minting will be saved

Example:
```sh
   cabal run create-token a71ab45081832acd58f6966b9b5d54027766b72d5db11340f70f9cc1772f90a0 2 TokenName TokenName
```

## Project structure 
  - `./Protocol_Specification` - documentation for Cardano scripts
  - `./src` - on-chain scripts
  - `./tests` - tests that involve off-chain code written in Lucid library
    - `offchain.ts`
    - `secret.ts` 
  - `./scripts` - pre-compiled plutus scripts used for tests

