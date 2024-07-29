The tests were performed on the Preprod test network

# Instructions 

To run tests, specify `secret.ts` file in this (`tests`) folder.

In `secret.ts` the followig constants must be specified:  
  - blockfrostKey
  - owner1PrivateKey
  - owner2PrivateKey
  - owner3PrivateKey

## Run tests with pre-compiled scripts

In this (`tests`) directory run:
```sh
    npm i
    npm i ts-node-esm
    ts-node-esm offchain.ts
```

## Run tests with new scripts

To change scripts, from the root directory enter `nix-shell` and run `cabal run create-token` with needed arguments. More info about arguments, what is needed to create Plutus Scripts can be found in this [README.MD](../README.md)


Enter `tests` directory and run:
```sh
    npm i
    npm i ts-node-esm
    ts-node-esm offchain.ts
```


# Performed tests
Off-chain code is written in Lucid library.Testing method is Smoke Testing.

Initially `main()` function in `offchain.ts` module checks for signatures of owner1, owner2 and min. threshold number that is equal to 2. Min. threshold number is a number of required signatures to mint tokens, add owner or remove owner.

Further checks are:
1. Owner1 mints tokens with owner1 and owner2 signatures
2. Not only owner can mint tokens, if transaction is signed by owner1 and owner2 too
3. Owner1 can freely burn their tokens without signatures from others
4. Add owner3 with signatures of owner1 and owner2
5. Mint tokens with signatures of all (three) owners (but threshold is still 2)
6. Remove owner1 with signatures of owner2 and owner3
7. Mint tokens with signatures of owner2 and owner3
8. Add owner1 with signatures of owner2 and owner3
9. Update threshold number to 3
10. Mint tokens with signatures of owner1, owner2, owner3
11. Set datum from output with NFT to initial state. Initial state datum's threshold equals to 2 and signatures are required only from owner1 and owner2