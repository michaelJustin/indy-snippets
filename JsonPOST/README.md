# JSON POST example

A minimum example which sends a HTTP POST with TIdHTTP to the httpbin web service.

## HTTPS support

The Indy (Internet Direct) TIdHTTP now creates a default SSLIOHandler when requesting an HTTPS url. This makes TIdHTTP a little easier to use “out of the box”. (blog article).
The code sends a hard-coded JSON object as POST body to a secure url. 
Note: if you need to customize the SSLVersions used, or specify certificates/keys, or use status/password event handlers, then you will still have to explicitly assign an SSLIOHandler component to the TIdHTTP.IOHandler property before sending an HTTPS request.


## Blog article 
https://mikejustin.wordpress.com/2015/03/14/indy-10-6-https-post/
