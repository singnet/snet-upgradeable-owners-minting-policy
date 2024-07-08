# snet-upgradeable-owners-minting-policy


## Instructions 

Built on top of plutus-apps v1.1.0, `cabal.project` refers to its packages. Make sure to install `plutus-apps` first.

### Compile & interact with scripts
From nix shell at the root directory run:

```sh
    cabal update
    cabal repl
```

## Project structure 
  - `./Protocol_Specification` - documentation for Cardano scripts
  - `./src` - on-chain scripts
  - `./tests` - tests that involve off-chain code written in Lucid library
    - `offchain.ts`
    - `secret.ts` 
  - `./scripts` - pre-compiled plutus scripts used for tests

