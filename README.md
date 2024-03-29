# rChain
## rChain - Blockchain implementation without PoW

> *WIP*

> *WARNING:* Proof of concept. Not ready for production use.

> *WARNING:* The chain created is not persistent. Any data will be lost on R environment cleanup.

### What is rChain?
In the past few years several DLT implementations emerged. Most of them work based on Proof-of-Work algorithms, consisting in several 'adversaries' competing to solve a computationally complex puzzle. The winner has the right to 'mine' a new block.
While this allows for distributed consensus generation and distributed secure time stamping of transactions, it makes the protocol inefficient for internal or small scale projects or any project where participants somehow already trust each other (private chains).

`rChain` uses a different approach to trust generation. It assumes the node running the chain is trusted by the participants (similar to a notary). Such node a pair of trusted keys which it uses to digitally sign any item/transaction and any block. While the private key is kept safe from attackers, any attempt to change the transactions or blocks will void the digital signature and the chain will fail to validate.

#### Pros
1. Energy Efficient, since there is no need for several nodes to compete on computing a suitable hash within the required difficulty.
2. Fast block generation, since a block can be generated almost instantaneously upon request. Precise block timing is possible.
3. Anyone with the public key can validate the digital signatures behind transactions/chain integrity.

#### Cons
1. Not distributed.
2. Trust limited by trust in chain owner (i.e., in the chain's private key owner).
