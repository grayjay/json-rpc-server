json-rpc-server
===============
[![Hackage](https://img.shields.io/hackage/v/json-rpc-server.svg?style=flat)](https://hackage.haskell.org/package/json-rpc-server) [![Build Status](https://travis-ci.org/grayjay/json-rpc-server.svg?branch=master)](https://travis-ci.org/grayjay/json-rpc-server)

An implementation of the server side of JSON-RPC 2.0. See <http://www.jsonrpc.org/specification>.  json-rpc-server uses ByteString for input and output, leaving the choice of transport up to the user.  It can be used with [json-rpc-client](http://hackage.haskell.org/package/json-rpc-client) to create a client and server that communicate with the same methods.