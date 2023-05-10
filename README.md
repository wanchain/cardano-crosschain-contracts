# cardano-crosschain-contracts
Plutus contracts for Cardano cross-chain infrastructure. And all contracts is based on plutus v2.

note:
Due to optimization requirements, token assets are used instead of ada utxo as the identification for proxing logical checking ,when spending from Treasury or Minting Mapping token.
So there are two separate instancies of CheckToken, one is for TreasuryCheck and another is for MintCheck.