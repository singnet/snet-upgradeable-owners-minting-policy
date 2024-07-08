import { 
    Address, 
    Blockfrost, 
    C, 
    Constr, 
    Data, 
    Lucid, 
    PolicyId, 
    Redeemer, 
    ScriptType, 
    SpendingValidator, 
    TxHash, 
    Unit, 
    fromText, 
    getAddressDetails 
} from "lucid-cardano"; 

import { Buffer } from 'buffer'


const { PrivateKey } = C;
import { 
    blockfrostKey, 
    owner1PrivateKey, 
    owner2PrivateKey, 
    owner3PrivateKey
} from "./secret.js"
import nftPlutusScript from "../scripts/nft.json" assert { type: "json" }
import tokenPlutusScript from "../scripts/token.json" assert { type: "json" }
import ValidatorPlutusScript from "../scripts/validator.json" assert { type: "json" }

export type oref = {
  txHash: string,
  outputIndex: number 
}

export const threadOref: oref = {
  txHash: "928270c029644e6dcdfe2c63ca55e65dfbd3d7827ccac76abf63796329bb6afe",
  outputIndex: 0
}

// due to delays in blockchain
function delay(ms: number) {
    return new Promise( resolve => setTimeout(resolve, ms) );
}


// set blockfrost endpoint
const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        blockfrostKey
    ),
    "Preprod"
);

const owner1Key = PrivateKey.from_normal_bytes(
    Buffer.from(owner1PrivateKey, "hex") 
)
const owner2Key = PrivateKey.from_normal_bytes(
     Buffer.from(owner2PrivateKey, "hex") 
)
const owner3Key = PrivateKey.from_normal_bytes(
    Buffer.from(owner3PrivateKey, "hex") 
)

const owner1KeyBech32 = owner1Key.to_bech32()
const owner2KeyBech32 = owner2Key.to_bech32()
const owner3KeyBech32 = owner3Key.to_bech32()

const addr2: Address = await (lucid.selectWalletFromPrivateKey(owner2KeyBech32)).wallet.address()
const addr3: Address = await (lucid.selectWalletFromPrivateKey(owner3KeyBech32)).wallet.address()
console.log("addr3: ", addr3)
const addr1: Address = await (lucid.selectWalletFromPrivateKey(owner1KeyBech32)).wallet.address()

const pkh1: string = getAddressDetails(addr1).paymentCredential?.hash || "";
const pkh2: string = getAddressDetails(addr2).paymentCredential?.hash || "";
const pkh3: string = getAddressDetails(addr3).paymentCredential?.hash || "";


function completeScript(policy: string) : { type: ScriptType; script: string; } {
    return {
        type: "PlutusV2",
        script: policy
    };
};


/*   NFT   */
const nftPolicy = nftPlutusScript["cborHex"]
const nftPolicyId: PolicyId = lucid.utils.mintingPolicyToId(completeScript(nftPolicy));
const nftUnit: Unit = nftPolicyId + fromText("Thread_NFT");
const emptyRedeemer = Data.void();


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

// Mint thread NFT for `upgradeableOwnersValidator`
export async function mintNFT(): Promise<TxHash> {
    const utxos = await lucid.wallet.getUtxos()
    const utxo = utxos.find(
        (utxo) =>
          utxo.txHash === threadOref.txHash && utxo.outputIndex === threadOref.outputIndex
    )
    if (!utxo) throw new Error("Utxo is required to mint NFT")

    const tx = await lucid
      .newTx()
      .collectFrom([utxo])
      .mintAssets({ [nftUnit]: 1n }, emptyRedeemer)
      .validTo(Date.now() + 100000)
      .payToContract(
        validatorAddress, 
        {inline: Data.to(testDatum, MultiSigDatum)},
        {lovelace: 2000000n, [nftUnit]: 1n}
      )
      .attachMintingPolicy(completeScript(nftPolicy))
      .complete();
  
    const signedTx = await tx.sign().complete();
  
    const txHash = await signedTx.submit();
    return txHash;
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
      .validTo(Date.now() + 100000)
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
      .validTo(Date.now() + 100000)
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
      .validTo(Date.now() + 100000)
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
      .validTo(Date.now() + 100000)
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
      .validTo(Date.now() + 100000)
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

      .validTo(Date.now() + 100000)
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
      .validTo(Date.now() + 100000)
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
      .validTo(Date.now() + 100000)
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
    .validTo(Date.now() + 100000)
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
      //console.log("mint NFT: ", await mintNFT())
      //await delay(secondsToWait)

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

