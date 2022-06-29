## SuperToken Deployer

This is a single-file Dapp for deploying SuperToken wrappers of existing ERC20 tokens.
Requires a browser with injected web3 provider (e.g. Metamask) and a local webserver.
If you have python installed, you can start a webserver with the document root set to the current directory with
```python
python -m SimpleHTTPServer 1337
```
Then navigate to http://localhost:1337/supertoken-deployer.html

Alternatively, you can also find it on IPFS: [QmbjeSLXfmePAnWRBhkhajCpdu58Xsn7BqQwj8pSxRS9d8](https://ipfs.io/ipfs/QmaCBCARcUthLcG1sNYC8SAE7okuDUZqbythvWuYozBpgC)

## Stream Closer

This is a single-file Dapp for closing streams / deleting flows.
The flow parameters _token_, _sender_ and _receiver_ can be set either in the UI or provided as URL parameters.
If you have python installed, you can start a webserver with the document root set to the current directory with
```python
python -m SimpleHTTPServer 1337
```

Then navigate to http://localhost:1337/stream-closer.html

Alternatively, you can also find it on IPFS: [QmRJmHatYiGgGsXYCsYZdqPMx4dd2qbMskMJvXGWZ53oJJ](https://ipfs.io/ipfs/QmVQ4B9MyEpr3i39XiEZ4qefuqMKv5HnfLPPB42r19mhEi)

In order to set the parameter in the URL, use this format:
stream-closer.html?chainId=<chainId>&token=<tokenAddress>&sender=<senderAddress>&receiver=<receiverAddress>
