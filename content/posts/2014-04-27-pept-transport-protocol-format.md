---
title: 'PEPT: Presentation, Encoding, Protocol, Transport'
tags: pept
---

Erik Meijer [points out](https://twitter.com/headinthebox/statuses/460414363166576640) a typo (or conceptual error) in [an FPComplete article](https://t.co/77tsmmMADr) that calls JSON a protocol and HTTP a format.

The [PEPT](http://bit.ly/1tRj5TY) remoting architecture considers:

-   a transport to be something that moves bits from one location to another
    
    -   a transport is something where the program does *not* manipulate header bits
    
    -   e.g., most apps do not touch TCP/IP header bits, they just write/read from the TCP/IP streams

-   a remoting protocol is something where, besides moving bits, the program *will* manipulate the header bits
    
    -   e.g., HTTP is a protocol for REST
    
    -   e.g., HTTP is a transport for SOAP (in general)

-   a remoting format is
    
    -   a serial encoding of the application data
    
    -   a serial encoding of protocol and/or transport headers
    
    -   e.g., JSON is a common application format for REST
    
    -   e.g., XML is both the application format and the protocol format for SOAP
    
    -   e.g., [CDR](http://en.wikipedia.org/wiki/Common_Data_Representation) is both the application format and the protocol format for CORBA IIOP

There are, of course, special cases, but the above taxonomy provides a useful separation of concerns.
