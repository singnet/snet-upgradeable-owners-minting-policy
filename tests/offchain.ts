import { 
    Address, 
    C, 
    Constr, 
    Data, 
    Emulator, 
    Lucid, 
    MintingPolicy, 
    OutRef, 
    PolicyId, 
    Redeemer, 
    Script, 
    ScriptType, 
    SpendingValidator, 
    TxHash, 
    Unit, 
    applyParamsToScript, 
    fromText, 
    getAddressDetails
} from "lucid-cardano"; 

const { PrivateKey } = C;
import nftPlutusScript from "../scripts/nft.json" assert { type: "json" }
import nftUnappliedPlutusScript from "../scripts/nft_unapplied.json" assert { type: "json" }
import tokenPlutusScript from "../scripts/token.json" assert { type: "json" }
import tokenUnappliedPlutusScript from "../scripts/token_unapplied.json" assert { type: "json" }
import ValidatorPlutusScript from "../scripts/validator.json" assert { type: "json" }
import ValidatorUnappliedPlutusScript from "../scripts/validator_unapplied.json" assert { type: "json" }

export type oref = {
  txHash: string,
  outputIndex: number 
}

export const threadOref: oref = {
  txHash: "f160ee01ff83321e37289837d6998273c4629667fb8f85b1a938fab81e01806b",
  outputIndex: 1
}

// due to delays in blockchain
function delay(ms: number) {
    return new Promise( resolve => setTimeout(resolve, ms) );
}

// used for utxo for oneshot minting policy
const mp0Key = PrivateKey.generate_ed25519()
// owner keys
const owner1Key = PrivateKey.generate_ed25519()
const owner2Key = PrivateKey.generate_ed25519()
const owner3Key = PrivateKey.generate_ed25519()

const mp0KeyBech32 = mp0Key.to_bech32()
const owner1KeyBech32 = owner1Key.to_bech32()
const owner2KeyBech32 = owner2Key.to_bech32()
const owner3KeyBech32 = owner3Key.to_bech32()

const addr0 = await (await Lucid.new(undefined, "Custom"))
  .selectWalletFromPrivateKey(mp0KeyBech32).wallet.address();
const addr1 = await (await Lucid.new(undefined, "Custom"))
  .selectWalletFromPrivateKey(owner1KeyBech32).wallet.address();
const addr2 = await (await Lucid.new(undefined, "Custom"))
  .selectWalletFromPrivateKey(owner2KeyBech32).wallet.address();
const addr3 = await (await Lucid.new(undefined, "Custom"))
  .selectWalletFromPrivateKey(owner3KeyBech32).wallet.address();

const pkh1: string = getAddressDetails(addr1).paymentCredential?.hash || "";
const pkh2: string = getAddressDetails(addr2).paymentCredential?.hash || "";
const pkh3: string = getAddressDetails(addr3).paymentCredential?.hash || "";

const emulator = new Emulator([
  {
    address: addr0,
    assets: { lovelace: 3_000_000_000n },
  },
  {
    address: addr0,
    assets: { lovelace: 5_000_000n }, // collateral
  },
  {
    address: addr1,
    assets: { lovelace: 3_000_000_000n },
  },
  {
    address: addr1,
    assets: { lovelace: 5_000_000n }, // collateral
  },
  {
    address: addr2,
    assets: { lovelace: 3_000_000_000n },
  },
  {
    address: addr2,
    assets: { lovelace: 5_000_000n }, // collateral
  },
]);

const lucid = await Lucid.new(emulator);

function completeScript(policy: string) : { type: ScriptType; script: string; } {
    return {
        type: "PlutusV2",
        script: policy
    };
};

// TODO: use
let outRefToData = (outRef: OutRef) => Data.to( 
  new Constr(0, [
    new Constr(0, [outRef.txHash]),
    BigInt(outRef.outputIndex)
  ])
  );

const NFT_TOKEN_NAME = "Thread_NFT";

// TODO: use
const nftParamsToData = (policy : Script) : Data => { 
  // console.log("nft hash?: ", lucid.utils.mintingPolicyToId(policy));
  return Data.to( new Constr(0, [ 
     lucid.utils.mintingPolicyToId(policy), // todo: check if its already hex encoded with above print!!!
    fromText(NFT_TOKEN_NAME)
   ]))
}

const TokenNameSchema = Data.Bytes();

type TokenNameData = Data.Static<typeof TokenNameSchema>;
const TokenNameData = TokenNameSchema as unknown as TokenNameData;

// TODO: use
const applyNftPolicy = (outRef : OutRef) => applyParamsToScript(
    nftUnappliedPlutusScript["cborHex"],
    [
      outRefToData(outRef),
      // Data.to(
        fromText(NFT_TOKEN_NAME),
      //   TokenNameData,
      // )
    ],
  )

// TODO: use
const applyTokenPolicy = (nftParams : Data, token_name: string) => applyParamsToScript(
  tokenUnappliedPlutusScript["cborHex"],
  [ nftParams,
    Data.to( fromText(token_name) )
  ],
)

// TODO: use
const applyValidator = (nftParams : Data) => applyParamsToScript(
  nftUnappliedPlutusScript["cborHex"],
  [ nftParams
  ],
)

/*   NFT   */
const nftPolicy = nftPlutusScript["cborHex"]
const nftPolicyId: PolicyId = lucid.utils.mintingPolicyToId(completeScript(nftPolicy));
const nftUnit: Unit = nftPolicyId + fromText("Thread_NFT");
const emptyRedeemer = Data.void();
// this changes to () but encoded as PlutusData, that ends up being `Constr 0 []`
const plutusDataUnit = Data.to(new Constr(0, []))

/*   Token   */
const tokenPolicy = tokenPlutusScript["cborHex"]
const tokenScript: SpendingValidator = completeScript(tokenPolicy)
const tokenPolicyId: PolicyId = lucid.utils.mintingPolicyToId(tokenScript);
const tokenUnit: Unit = tokenPolicyId + fromText("TToken");


/*   Validator   */
const validator = ValidatorPlutusScript["cborHex"]
const validatorScript: SpendingValidator = completeScript(validator)
const validatorAddress: Address = lucid.utils.validatorToAddress(validatorScript)
const redemeerTokenMint =  Data.to(new Constr(0, [])) as Redeemer
const redemeerAddOwner = (pkh: string, threshold: number) => Data.to(new Constr(1, [String(pkh), BigInt(threshold)])) as Redeemer
const redemeerRemoveOwner = (pkh: string, threshold: number) => Data.to(new Constr(2, [String(pkh), BigInt(threshold)])) as Redeemer
const redemeerUpdateThreshold = (threshold: number) => Data.to(new Constr(3, [BigInt(threshold)])) as Redeemer


const isTxValidated = async(txHash: string) => {
    if(!txHash) 
        throw ("No transaction hash provided")
    if(!(typeof 'TxHash' == typeof txHash)) 
        throw("Unappropriate type of transaction hash")
    if(!await lucid.awaitTx(txHash)) 
        throw ("Transaction wasn't validated")
}

type ProtocolScripts = {
  nftPolicy: MintingPolicy,
  tokenPolicy: MintingPolicy,
  validatorScript: SpendingValidator
};

// Mint thread NFT for `upgradeableOwnersValidator`
export async function mintNFT(): Promise<{ txHash: TxHash, scripts : ProtocolScripts }> {
    // TODO: now this function should probably return already applied scripts
    const utxos = await lucid.wallet.getUtxos()
    const utxo = utxos[0];
    if (!utxo) throw new Error("Utxo is required to mint NFT")

    const nftPolicy = completeScript(applyNftPolicy( {
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
    }));

    // const nftParams = nftParamsToData(nftPolicy);

    // const tokenPolicy = completeScript(applyTokenPolicy(nftParams, "TToken" ));

    // const validatorScript: SpendingValidator = completeScript(applyValidator( nftParams ));

    const nftPolicyId: PolicyId = lucid.utils.mintingPolicyToId(nftPolicy);
    const nftUnit: Unit = nftPolicyId + fromText("Thread_NFT");

    const validatorAddress: Address = lucid.utils.validatorToAddress(validatorScript)

    const tx = await lucid
      .newTx()
      .collectFrom([utxo])
      .mintAssets({ [nftUnit]: 1n }, plutusDataUnit)
      .validTo(emulator.now() + 100000)
      .payToContract(
        validatorAddress,
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n}
      )
      .attachMintingPolicy(nftPolicy)
      .complete();

    console.log('built');

    const signedTx = await tx.sign().complete();

    const txHash = await signedTx.submit();
    // return { txHash , scripts : { nftPolicy, tokenPolicy, validatorScript } };
    return { txHash , scripts : { nftPolicy, tokenPolicy : nftPolicy, validatorScript : nftPolicy } };
}

/*   Datum Scheme   */
const MultiSigDatumSchema = Data.Object({
    owners: Data.Array(Data.Bytes()),
    minSigs: Data.Integer()
  });

type MultiSigDatum = Data.Static<typeof MultiSigDatumSchema>;
const MultiSigDatum = MultiSigDatumSchema as unknown as MultiSigDatum;

// Global variable, init state
let testDatum: MultiSigDatum = {
    owners: [String(pkh1), String(pkh2)],
    minSigs: 2n
};  

export async function mintTokenWithOwners12(token: Unit, amount: bigint) {
    console.log(" - mint with owner 1 and owner 2");
    console.log("validator address: ", validatorAddress)
    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 
    
    const tx = await lucid
      .newTx()
      .mintAssets({ [token]: amount }, emptyRedeemer)
      .collectFrom([utxWithThread], redemeerTokenMint)
      
      .addSignerKey(pkh1)  
      .addSignerKey(pkh2)  

      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})
      .validTo(emulator.now() + 100000)
      .attachMintingPolicy(tokenScript)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner2KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `mintTokenWithOwners12`: " + txHash + "\n");
}


// helper
export async function transfer(datum: MultiSigDatum, to: Address, unit: Unit, amount: bigint) {
    console.log(" - transfer");

    const tx = await lucid
        .newTx()
        .payToContract(
            to, 
            {inline: Data.to(datum, MultiSigDatum)},
            {lovelace: 2000000n, [unit]: amount}
        )
        .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("txHash from `transfer`: " + txHash + "\n");
}


// 3d party can submit minting transaction to network and receive tokens
// but signatures from owner1 and owner2 are required
export async function notOwnerMinting(token: Unit, amount: bigint) {
    console.log("- mint with 2 owners, tx is submitted by another address");

    // not owner of token while running this function
    lucid.selectWalletFromPrivateKey(owner3KeyBech32)

    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 

    const tx = await lucid
      .newTx()
      .mintAssets({ [token]: amount }, emptyRedeemer)
      .collectFrom([utxWithThread], redemeerTokenMint)
      .addSignerKey(pkh3)  
      .addSignerKey(pkh1)  
      .addSignerKey(pkh2)  
      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})
      .validTo(emulator.now() + 100000)
      .attachMintingPolicy(tokenScript)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner1KeyBech32)
      .signWithPrivateKey(owner2KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    lucid.selectWalletFromPrivateKey(owner1KeyBech32)

    console.log("✅ txHash from `notOwnerMinting`: " + txHash + "\n");
}

export async function mintTokenWithOwners23(token: Unit, amount: bigint) {
    console.log(" - mint with owner 3 and owner 2");

    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 

    const tx = await lucid
      .newTx()
      .mintAssets({ [token]: amount }, emptyRedeemer)
      .collectFrom([utxWithThread], redemeerTokenMint)
      
      .addSignerKey(pkh2)  
      .addSignerKey(pkh3)  

      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})
      .validTo(emulator.now() + 100000)
      .attachMintingPolicy(tokenScript)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner2KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `mintTokenWithOwners23`: " + txHash + "\n");
}

// each token holder can burn their tokens without signatures from owners
export async function burnTokens(script: { type: ScriptType; script: string; }, unit: Unit, amount: bigint) {
    console.log("- burn tokens with amount, ", amount)

    const tx = await lucid
        .newTx()
        .mintAssets({[unit]: amount}, Data.void())
        .attachMintingPolicy(script)
        .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `burnTokens`: " + txHash + "\n");
}

export async function addOwner3WithOwners12() {
    console.log(" - add owner with 2 owners");
    testDatum.owners = [String(pkh3), String(pkh1), String(pkh2)]

    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 
    const tx = await lucid
      .newTx()
      .collectFrom([utxWithThread], redemeerAddOwner(pkh3, Number(testDatum.minSigs)))
      
      .addSignerKey(pkh1)  
      .addSignerKey(pkh2)  

      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})
      .validTo(emulator.now() + 100000)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner2KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `addOwner3WithOwners12`: " + txHash + "\n");
}

export async function addOwner1WithOwners23(threshold: number) {
    console.log(" - add owner1 with owner2 and owner3");
    testDatum.owners = [String(pkh1), String(pkh3), String(pkh2)]

    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 
    const tx = await lucid
      .newTx()
      .collectFrom([utxWithThread], redemeerAddOwner(pkh1, threshold))
      
      .addSignerKey(pkh2)  
      .addSignerKey(pkh3)  

      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})
      .validTo(emulator.now() + 100000)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner2KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `addOwner1WithOwners23`: " + txHash + "\n");
}

export async function mintTokenWithThreeOwners(token: Unit, amount: bigint) {
    console.log(" - mint with 3 owners");
    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 
    const tx = await lucid
      .newTx()
      .mintAssets({ [token]: amount }, emptyRedeemer)
      .collectFrom([utxWithThread], redemeerTokenMint)

      .addSignerKey(pkh1)  
      .addSignerKey(pkh2)  
      .addSignerKey(pkh3)  

      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})

      .validTo(emulator.now() + 100000)
      .attachMintingPolicy(tokenScript)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner2KeyBech32)
      .signWithPrivateKey(owner3KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `mintTokenWithThreeOwners`: " + txHash + "\n");
}


export async function removeFirstOwnerWithThreeOwners(threshold: number) {
    console.log(" - remove first owner with owners=3, threshold=2, signers are owner3 and owner2");
    testDatum.owners = [String(pkh3), String(pkh2)]

    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 
    const tx = await lucid
      .newTx()
      .collectFrom([utxWithThread], redemeerRemoveOwner(pkh1, threshold))
      
      .addSignerKey(pkh2)  
      .addSignerKey(pkh3)    

      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})
      .validTo(emulator.now() + 100000)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner2KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `removeFirstOwnerWithThreeOwners`: " + txHash + "\n");
}


export async function updateThresholdWithOwners23(newThreshold: number) {
    console.log(" - update threshold number, current threshold=",testDatum.minSigs, "new threshold=", BigInt(newThreshold));
    const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 
    
    testDatum.minSigs = BigInt(newThreshold)

    const tx = await lucid
      .newTx()
      .collectFrom([utxWithThread], redemeerUpdateThreshold(newThreshold))

      .addSignerKey(pkh1) 
      .addSignerKey(pkh2)  
      .addSignerKey(pkh3)    

      .payToContract(validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n})
      .validTo(emulator.now() + 100000)
      .attachSpendingValidator(validatorScript)
      .complete();

    const signedTx = await tx.sign()
      .signWithPrivateKey(owner2KeyBech32)
      .signWithPrivateKey(owner1KeyBech32)
      .complete();
    const txHash = await signedTx.submit();

    await isTxValidated(txHash)

    console.log("✅ txHash from `updateThresholdWithOwners23`: " + txHash + "\n");
}

export async function setToInitState(threshold: number) {
  console.log(" - setToInitState: remove owner3 & set threshold=2 in one tx");
  testDatum = { owners : [String(pkh1), String(pkh2)], minSigs: 2n }
  
  const utxWithThread = (await lucid.utxosAtWithUnit(validatorAddress, nftUnit))[0] 
  const tx = await lucid
    .newTx()
    .collectFrom([utxWithThread], redemeerRemoveOwner(pkh3, threshold))

    .addSignerKey(pkh1) 
    .addSignerKey(pkh2)  
    .addSignerKey(pkh3)    

    .payToContract(validatorAddress, 
      {inline: Data.to(testDatum, MultiSigDatum)},
      {lovelace: 2000000n, [nftUnit]: 1n})
    .validTo(emulator.now() + 100000)
    .attachSpendingValidator(validatorScript)
    .complete();

  const signedTx = await tx.sign()
    .signWithPrivateKey(owner2KeyBech32)
    .signWithPrivateKey(owner3KeyBech32)
    .complete();
  const txHash = await signedTx.submit();

  await isTxValidated(txHash)

  console.log("✅ txHash from `setToInitState`: " + txHash + "\n");
}


// can be adjusted. however, sometimes it can be not enough
const secondsToWait: number = 1000*40

const main = async() => {
    try {
      // this sets the wallet owner to the owner owning UTXO for one shot mint
      // NOTE: at certain point only single key is being used for wallet
      lucid.selectWalletFromPrivateKey(mp0KeyBech32)

      await delay(1000*5)
      console.log("mint NFT: ", await mintNFT())
      await delay(secondsToWait)

      // todo: need to use the applied scripts below

      console.log(
        "\n*****  Testing: MINTING    *****\n",
      ) 
      await mintTokenWithOwners12(tokenUnit, 10n)
      await delay(secondsToWait)

      await notOwnerMinting(tokenUnit, 7n)
      await delay(secondsToWait)


      console.log(
          "\n*****  Testing: BURNING    *****\n",
      )
      await burnTokens(tokenScript, tokenUnit, -3n)


      console.log(
        "\n*****  Testing: ADD owner & MINT tokens *****\n",
      )

      await addOwner3WithOwners12()
      await delay(secondsToWait)

      await mintTokenWithThreeOwners(tokenUnit, 5n)
      await delay(secondsToWait)


      console.log(
        "\n*****  Testing: REMOVE owner & MINT tokens  *****\n",
      )

      lucid.selectWalletFromPrivateKey(owner3KeyBech32)

      await removeFirstOwnerWithThreeOwners(2)
      await delay(secondsToWait)

      await mintTokenWithOwners23(tokenUnit, 14n)
      await delay(secondsToWait)

      console.log(
        "\n*****  Testing: ADD owner & UPDATE threshold  *****\n",
      )
      await addOwner1WithOwners23(2)
      await delay(secondsToWait)

      await updateThresholdWithOwners23(3)
      await delay(secondsToWait)

      lucid.selectWalletFromPrivateKey(owner1KeyBech32)
      await mintTokenWithThreeOwners(tokenUnit, 30n)

      console.log(
        "\n*****  Setting to initial state   *****\n",
      )
      //remove third owner and update threshold to `2` in one transaction 
      await setToInitState(2)
      await delay(secondsToWait)
    } catch(error) {
        console.log("Error while running tests: ", error)
   }
}


//console.log("Get wallet utxos: ", await lucid.wallet.getUtxos())

await main()

