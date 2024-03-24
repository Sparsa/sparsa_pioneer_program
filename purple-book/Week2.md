{-

(4) explain some of the ways hashing functions enable blockchain technology
Hasing functions can be used widely to enable blockchain technology.
Here I am listing a couple of the uses:
1. Data intigrity: One can use hasing to check if the all blocks are same. Even a minor change in the block changes the hashing information. One can quickly check the hasing info by computing the hash of the chain to confirm that there is no change in the existing chain.
2. Security: It is impossible to inverse hasing. So, it is not possible for any attacker to reverse compute and get the original data and manipulate the
3. Consensus: Hasing is extensively used in the consensus mechanism used by the block chains. Such as, Proof of Work and Proof of Stake (PoS). 
4. Address generation: One can use hasing to generate unique addresses for users of the block chain.



(5) briefly explain Bitcoin's UTXO model of transaction validation (separate from POW)
The UTTxO (Unspent Transaction Output)  is a method used in blockchains to validate trasactions and deterine the ownership of cryptocurrencies.
1. Transaction outputs: Every transaction on the blockchains creates one or more UTXO. Thse represents the crypto currencies recieved by recipeints, they also have extra information that can be verified.
2. Ownership verification: Users prove their ownership of the coins associated with their address using digital signatures.
3. Transactions (I/Os): when creating a new transaction users specify which UTXOs they want to spend, and where they want to spend their crypto assets. These UTXOs becomes the input of the transactions. Simlarly every output represents an UTXOs that is targated to the recipeitns address and have the amount of crypto asset to be transferred.

Overall, the UTXO odel provides a method for tracking the ownership of crypto assets, and also provides necessary information to validate the transactions.

(6) what is the structure of a Block in bitcoin and how does it relate to the 'blockchain' (merkle tree vs merkle list of merkle trees)
Normally, every block in the block chain contins the following informations:
### Block Header:
1. Previous block hash: Denotes the hash of the previous block in the block chain, this works like a linked list and this gives the chain like structure to it.
2. Root hash: This gives the hash of all the transactions in a block, this forms Merkle tree.
3. Time stamp: This denotes the time when the block is mined.
### List of transactions: 
This represents the list of transactions within a block. Each transactions contains inputs and outputs representing the source address and the destination address of the transcation.
### Merkle Tree:
The transactions within a block are arranged in a specific order, then it is hasked pairwise to form a tree structure known as Merkle tree.The root of this tree, also referred as Merkle root hash, is included in the block header.This helps to represent the transactions in the block compactly, allowing efficient verification of the transactions.
### Merkle list:
To reduce the size of block headers, list of Merkle trees are used. They are called Merkle list or header chain.Instead of referencing each blocks Merkle root hash, blocks are organized into a merkle tree structure and the root of this tree is included in the block headers of the subsequent blocks. This allows efficient verification.

(7) what problem/s are POW/POS trying to solve? discuss/compare (byzantine fault tolerance, reaching a single consensus on a p2p network)
#### Byzantine Fault: 
Byzantine fault is a well known issue with the distributed systems. The issue is reaching a consensus even if some of its nodes fail or behave otherwise.In a decentralized system these kind of faults are very common, infact there can be some intruders who may want to play as a Byzantine player for their own gain.
#### Reaching a single consensus on a P2P network:
In a peer to peer network, achieving consensus among participants is crucial to ensure that all nodes have the same view of th block chains state. Without a single consensus, the network risks becoming fragmented, leading to inconsistancies and potential double-spending of digital assets.
#### Comparison of PoW and PoS
Resource consumption: Power of Work (PoW) requries significant computational power and energy consumption in coparison to Proof of Stake (PoS).
Security: PoW is highly secure due to its resource-incentive nature, while PoS relies on economic incentives and the assumption that malicious players will loose more finantially.
Decentralization: PoW tends to promote decenralization by allowing anyone with the necessary hardware to participate the consensus process. While, PoS may lead to centralization among validators with large stakes.
Scalability: PoS is often considered more scalable than PoW since it does not require extensive computational resources, making it potentially more suitable for networks with high transaction volumes.
-} 
