---
title: the chain in blockchain explained
tags: haskell, blockchain
---

This post develops a simple blockchain with the goal of understanding
the basics of the chain.

setup:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Chain where

import           ClassyPrelude         as CP
import           Crypto.Hash.SHA256    as C  (hash)
import           Data.ByteString       as BS (concat)
import           Data.ByteString.Char8 as BS (pack)
import qualified Prelude               as P  (tail)
import           Data.Sequence         as S
import           Test.Hspec
```

The most straigtforward part of a blockchain is the chain itself, a
sequence of blocks:

```haskell
type Blockchain = Seq Block
```

<!-- MORE -->

For the purposes of this exposition, a block in the chain is:

```haskell
data Block =
  Block { bIndex     :: ! BIndex     -- ^ index of this block in the chain -- for debugging
        , bPrevHash  :: ! BHash      -- ^ hash of previous block
        , bTimestamp :: ! BTimestamp -- ^ when this block was created
        , bData      :: ! BData      -- ^ this block's data
        } deriving (Eq, Show)
```

where

```haskell
type BIndex     = Int
type BHash      = ByteString
type BTimestamp = ByteString
type BData      = ByteString
```

A hard-coded genesis block:

```haskell
genesisBlock :: Block
genesisBlock =
 let idx      = 0
     prevHash = "0"
     ts       = "2017-03-05 10:49:02.084473 PST"
     bdata    = "GENESIS BLOCK DATA"
 in Block idx prevHash ts bdata
```

begins the chain:

```haskell
genesisBlockchain :: Blockchain
genesisBlockchain  = S.singleton genesisBlock
```

For this exposition, the only purpose of the genesis block is to provide
a verifiable `prevHash` for the first "real" block that gets added to
the chain.

The chain is tamper-proof because each block contains a hash of of the
contents of the previous block:

```haskell
calculateHash :: BIndex -> BHash -> BTimestamp -> BData -> BHash
calculateHash i p t d = C.hash (BS.concat [BS.pack $ show i, p, t, d])
```

Since a block contains the hash of the previous block, once a block has
been added to a chain, the previous contents of the chain cannot be
altered without detection.

For example, using the following functions:

```haskell
addBlock :: BTimestamp -> BData -> Blockchain -> Blockchain
addBlock ts bd bc = bc |> makeNextBlock bc ts bd

makeNextBlock :: Blockchain -> BTimestamp -> BData -> Block
makeNextBlock bc ts bd =
  let (i, ph, _, _) = nextBlockInfo bc ts bd
  in Block i ph ts bd

nextBlockInfo :: Blockchain -> BTimestamp -> BData
              -> (BIndex, BHash, BTimestamp, BData)
nextBlockInfo bc ts bd =
  let prev = getLastCommittedBlock bc
      i    = bIndex prev + 1
      ph   = calculateHash (bIndex prev) (bPrevHash prev) (bTimestamp prev) (bData prev)
  in (i, ph, ts, bd)

getLastCommittedBlock :: Blockchain -> Block
getLastCommittedBlock bc = S.index bc (S.length bc - 1)
```

a new block may be added to the chain:

```haskell
t1 :: Spec
t1 =
  let newChain = addBlock "2017-06-11 15:49:02.084473 PST"
                          "June 11 data"
                          genesisBlockchain
  in describe "t1: add new block to chain" $ do
       it "increases length" $
         S.length newChain `shouldBe` S.length genesisBlockchain + 1
       it "does not change existing blocks" $
         getBlock newChain 0 `shouldBe` Just genesisBlock
```

where

```haskell
-- | Nothing if index out of range.
getBlock :: Blockchain -> BIndex -> Maybe Block
getBlock bc i = S.lookup i bc
```

The chained block hashes ensure the integrity of a blockchain.
Modification of a parts of any blocks in the chain is detected:

```haskell
t2 :: Spec
t2 =
  let newChain = addBlock "2017-06-12 15:49:02.084473 PST"
                          "June 12 data"
                          (addBlock "2017-06-11 15:49:02.084473 PST"
                                    "June 11 data"
                                    genesisBlockchain)
      altered1i  = (S.index newChain 1) { bIndex = 10 }
      badChain1i = S.update 1 altered1i newChain
      altered1p  = (S.index newChain 1) { bPrevHash = "0" }
      badChain1p = S.update 1 altered1p newChain
      altered1d  = (S.index newChain 1) { bData  = "altered June 11 data" }
      badChain1d = S.update 1 altered1d newChain
      altered12  = (S.index newChain 2) { bData  = "altered June 12 data" }
      badChain12 = S.update 2 altered12 newChain
  in describe "t2: valid blockchain" $ do
    it "invalid empty" $
      isValidBlockchain S.empty    `shouldBe` Left "empty blockchain"
    it "valid genesisblockchain" $
      isValidBlockchain genesisBlockchain
                                   `shouldBe` Right ()
    it "invalid genesisblock" $
      isValidBlockchain (S.singleton altered12)
                                   `shouldBe` Left "invalid genesis block"
    it "valid newChain" $
      isValidBlockchain newChain   `shouldBe` Right ()
    it "invalid bIndex 1" $
      isValidBlockchain badChain1i `shouldBe` Left "invalid bIndex 1"
    it "invalid bPrevHash 1" $
      isValidBlockchain badChain1p `shouldBe` Left "invalid bPrevHash 1"
    it "invalid bPrevHash 2" $
      isValidBlockchain badChain1d `shouldBe` Left "invalid bPrevHash 2"
    -- This shows that it IS possible to alter the last block in the chain.
    -- In a "real" blockchain, consensus and subsequent blocks added to
    -- the chain handles this case.
    it "changed last element in chain" $
      isValidBlockchain badChain12 `shouldBe` Right ()
```

where

```haskell
-- | Returns `Just ()` if valid.
-- otherwise `Left reason`
isValidBlockchain :: Blockchain -> Either Text ()
isValidBlockchain bc = do
  when (S.length bc == 0)                                 (Left "empty blockchain")
  when (S.length bc == 1 && S.index bc 0 /= genesisBlock) (Left "invalid genesis block")
  let elements = toList bc
  -- `sequence_` causes function to return on/with first `Left` value
  sequence_ (CP.map isValidBlock (CP.zip elements (P.tail elements)))
  return ()

-- | Given a valid previous block and a block to check.
-- | Returns `Just ()` if valid.
-- otherwise `Left reason`
isValidBlock :: (Block, Block) -> Either Text ()
isValidBlock (validBlock, checkBlock) = do
  when (bIndex    validBlock + 1 /= bIndex    checkBlock) (fail' "invalid bIndex")
  when (hashBlock validBlock     /= bPrevHash checkBlock) (fail' "invalid bPrevHash")
  return ()
 where
  fail' msg   = Left (msg <> " " <> tshow (bIndex validBlock + 1))
  hashBlock b = calculateHash (bIndex b) (bPrevHash b) (bTimestamp b) (bData b)
```

The above is the essence of the chain in blockchain.

future expositions
------------------

Note that there was no discussion of

-   how to decide to add a block to a chain (e.g., mining, consensus)
-   merkle trees (i.e., an efficient, tamper-proof way to structure
    `bData` to hold more than one "transaction")
-   smart contracts (i.e., how to interpret the `bData` field in a
    `Block`)

summary
-------

A blockchain is a list of blocks, where each block

-   contains data that may be interpreted as smart contracts
-   contains the hash of the previous block
-   the chained hashes makes the chain tamper-proof

```haskell
t3 :: Spec
t3 =
  let withOne = addBlock "2017-06-11 15:49:02.084473 PST"
                         "June 11 data"
                         genesisBlockchain
      withTwo = addBlock "2017-06-12 15:49:02.084473 PST"
                         "June 12 data"
                         withOne
  in describe "t3: list" $
     it "withTwo" $
     withTwo `shouldBe`
     S.fromList
     [ Block { bIndex = 0
             , bPrevHash = "0"
             , bTimestamp = "2017-03-05 10:49:02.084473 PST"
             , bData = "GENESIS BLOCK DATA"
             }
     , Block { bIndex = 1
             , bPrevHash = "'\234-\147\141\"\142\235\CAN \246\158<\159\199s\174\\\225<\174\188O\150oM\217\DC3'\237\DC4n"
             , bTimestamp = "2017-06-11 15:49:02.084473 PST"
             , bData = "June 11 data"
             }
     , Block { bIndex = 2
             , bPrevHash = "\145\238k24\175\147I\EOT\208\204\210\190s\192<b:\SOH\215\DC1\254)\173\EOT\186\220\US\SYNf\191\149"
             , bTimestamp = "2017-06-12 15:49:02.084473 PST"
             , bData = "June 12 data"
             }
     ]
```

source code and discussion
--------------------------

Thanks to Ulises Cerviño Beresi, Heath Matlock, Alejandro Serra Mena and Mark Moir
for pre-publication feedback.

The code for this exposition is available at :
[github](https://github.com/haroldcarr/blockchain-explained/tree/master/a001-the-chain-in-blockchain)

run the code: `stack test`

A discussion is at:
[reddit](https://www.reddit.com/r/Bitcoin/comments/6i876p/the_chain_in_blockchain_explained/)
