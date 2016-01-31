# Server-side message push

## Delphi 2009 VCL example

The program contains a server and a client. 
The server code uses a TIdTCPCustomServer subclass which waits for a random time and then sends a string to the client.
The client code uses a TThread subclass to run asynchronously without blocking the main VCL thread. It contains a private TIdTCPClient instance, and periodically tries to receive a string from the connection.

## Blog post

https://mikejustin.wordpress.com/2014/04/19/indy-10-tidtcpserver-server-side-message-push-example/
