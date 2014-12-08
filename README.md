# Erlang implementation of a socks5 proxy server

Socks5 proxy implemented in Erlang using [Ranch] (https://github.com/ninenines/ranch) TCP server. <br/>
Partially [RFC 1928] (https://www.ietf.org/rfc/rfc1928.txt) compliant.
# Available features
* TCP clients
* CONNECT and BIND commands
* IPv4 and FQDN
* username/password authentication (forced if available for client)
* salted hash password storage
* [list] (https://drive.google.com/file/d/0Bx4AnavqdePBeWZDakR2LWxEXzdKbm5WR08xUzFDXzB1QmE0/view?usp=sharing) of "registered" users

# Quick start
 <pre> make deps && make run </pre>
