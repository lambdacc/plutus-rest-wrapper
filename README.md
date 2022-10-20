# plutus-rest-wrapper
REStful wrapper for Plutus offchain

For dApps designed with a Plutus smart contracts backend, it will add a lot of utility to have a RESTful abstraction for the off chain code. A frontend can interact with the PAB to invoke the endpoints exposed via the Plutus off-chain code but the PAB cannot return a response back to the calling frontend. A RESTful response from the Plutus off-chain will provide access to attributes deduced with Plutus. For example, the script address of a parameterized Plutus contract.

Benefits:

A convenient way to bridge the frontend/backend for DApps with off-chain Plutus. The RESTful API can be used to fetch at runtime:

<ul>
<li>:heavy_check_mark: Plutus pub key hash for a given bech32 address</li>
<li>- [ ]  Serialized datum</li>
<li>- [ ]  Datum hash as encoded by Plutus</li>
<li>- [ ]  Parameterized script address</li>
<li>- [ ]  Datum hash as encoded by Plutus</li>
<li>- [ ]  CBOR encoded script generated with Plutus</li>
</ul>

What is listed above is not an exhaustive set of use cases for getting attributes from the off-chain code. However they go a long way to simplify the frontend-backend-Plutus interaction for a dApp.


To call out a related case, there are differences in the way data serialization is done by different tools like cardano-cli, serialization libs and Plutus. Unless identical serialized bytes are used the corresponding hashes turn out to be different. This leaves an inconsistency across toolsets and can become inconvenient to dApps that use more than one of the tools in their tech stack. This RESTful abstraction will make it easier to employ Plutus Application Backend (PAB) running Plutus smart contracts in the dApp stack. 

