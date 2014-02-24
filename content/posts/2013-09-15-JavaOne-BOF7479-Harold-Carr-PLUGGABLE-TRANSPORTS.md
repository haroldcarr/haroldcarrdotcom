---
title: SOAP over WebSockets and InfiniBand with JAX-WS Pluggable Transports
tags: soap, jax-ws, disi, websockets, infiniband
---

My presentation at JavaOne 2013.

ABSTRACT

The JAX-WS standard includes APIs for using POJOs or XML for remote
messages.  But it does not include APIs for letting the user control
the transport.  This BoF discusses adding pluggable transport APIs to
the JAX-WS standard.

This BoF shows a candidate pluggable transport mechanism for JAX-WS
that enables one to use other transports besides HTTP.  In particular,
it shows the benefits of using WebSockets and InfiniBand transports
for SOAP message exchanges.

<!-- MORE -->

SUMMARY

Background:

Besides being able to send and receive remote method calls with Java
objects (POJOs), JAX-WS has APIs that enable one to operate at the XML
level.  "Dispatch" is available on the client-side to feed XML into
JAX-WS for soap-processing (e.g., handling security).  The message is
then sent using the transport built into the system. After the response
is received and processed, the XML is given back to Dispatch.
Dispatch also has an asynchronous mode.

The service-side has a similar "Provider" API, except Provider does
not have an asynchronous mode.  However, the JAX-WS specification does
not have corresponding APIs for giving the user control of the
transport in a similar manner.

Pluggable Transports:

Asynchronous APIs for client and service transport are shown, as well
as APIs for plugging those transports into the underlying JAX-WS
system.  Using those APIs, the BoF includes a demonstration of
plugging in WebSocket, InfiniBand and RDMA transports into an existing
system.

The benefits of these transports are shown, such as:

- the ability to use an alternate transport if the vendor does not supply it
- ability to use binary encodings
- long lasting connections
- performance (with numbers from production environments)
- direct, application-to-application, zero-copy memory access that
  by-passes the operating system

The benefits are not without drawbacks, such as:

- connection management is now the responsibility of the transport
  writer instead of the platform
- memory management for zero-copy

These APIs have been used in a production environment.



# me, disclaimer, copyright

## BOF7479 : SOAP over WebSockets and InfiniBand with JAX-WS Pluggable Transports

Harold Carr

-   architect of SOAP Web Services Technology at Oracle


    Last Modified : 2013 Dec 15 (Sun) 13:15:19 by carr.

## disclaimer, copyright

THE FOLLOWING IS INTENDED TO OUTLINE OUR GENERAL PRODUCT DIRECTION. IT
IS INTENDED FOR INFORMATION PURPOSES ONLY, AND MAY NOT BE INCORPORATED
INTO ANY CONTRACT. IT IS NOT A COMMITMENT TO DELIVER ANY MATERIAL,
CODE, OR FUNCTIONALITY, AND SHOULD NOT BE RELIED UPON IN MAKING
PURCHASING DECISIONS. THE DEVELOPMENT, RELEASE, AND TIMING OF ANY
FEATURES OR FUNCTIONALITY DESCRIBED FOR ORACLE'S PRODUCTS REMAINS AT
THE SOLE DISCRETION OF ORACLE.

-   Copyright \copyright 2013 Oracle and/or its affiliates.

# intro



## what you will learn

-   client APIs to enter/exit JAX-WS
    
    -   for request processing
        
        -   enter: `DispatcherRequest`; exit: `ClientRequestTransport`
    
    -   for response processing
        
        -   enter: `ClientResponseTransport`; exit:  `DispatcherResponse`

-   service APIs to enter/exit JAX-WS
    
    -   for request processing
        
        -   enter: `ServiceRequestTransport`; exit:  `ProviderRequest`
    
    -   for response processing
        
        -   enter: `ProviderResponse`; exit:  `ServiceResponseTransport`

-   example dual HTTP/WebSocket transport (uses JSR-356)

-   potential InfiniBand transport

-   you will not learn WebSocket nor InfiniBand in detail

## motivation: SOAP passing thru intermediary


![](../images/presentations/2013-09-15-JavaOne/motivation.png)

## JAX-WS POJO usage


![](../images/presentations/2013-09-15-JavaOne/pojo-usage.png)

## JAX-WS XML usage


![](../images/presentations/2013-09-15-JavaOne/xml-usage.png)

## JAX-WS standard includes/lacks

-   JAX-WS standard
    
    -   includes APIs for
        
        -   POJO  : `@WebServiceRef`, `@WebService`, &#x2026;
        
        -   XML   : `Dispatch`, `Provider`, &#x2026;
        
        -   async : `Dispatch.invokeAsyc`
    
    -   does **not** include APIs
        
        -   user transport   : `WebSockets`, `InfiniBand`, &#x2026;
        
        -   async everywhere : async `Provider`, &#x2026;
        
        -   thread guarantees

-   propose
    
    -   adding pluggable async transport APIs
    
    -   adding async `Dispatch` and `Provider`
    
    -   thread guarantees

## proposal


![](../images/presentations/2013-09-15-JavaOne/proposal.png)

## benefits

-   ability to use alternate transports
    
    -   e.g., not supplied by vendor/implementation
    
    -   performance
        
        -   e.g., InfiniBand direct, app-to-app, zero-copy memory access by-passes OS

-   use "last minute" binary encodings

-   long lasting connections

Drawbacks

-   connection management responsibility of transport impl instead of platform

-   memory management for zero-copy

# programming with existing JAX-WS



## POJO example

    @WebService
    public class Hello {
      @Resource
      protected WebServiceContext context;
      @WebMethod
      public String hello(String name) { ... }
    }
    
    public class HelloClient {
      @WebServiceRef(wsdlLocation="...?wsdl")
      HelloService service;
      ...
        final Hello port = service.getHelloPort();
        ((BindingProvider)port).getRequest/ResponseContext();
        final String response = port.hello(av[0]);

## POJO PROS/CONS

PROS

-   easy to use/deploy

-   client `port` / `BindingProvider` access to request/response context

-   service access to request context

CONS

-   client `context.put` sticky : not per-request

-   no access to response context on service side

-   entire XML marshaled to/from POJOs

-   not async

-   no access to XML

-   no thread guarantees

-   cannot "own" transport

## `Dispatch` / `Provider` example

    @ServiceMode(value=Service.Mode.PAYLOAD) // MESSAGE
    @WebServiceProvider(serviceName="HelloService", portName="HelloPort", ...)
    public class HelloImpl implements Provider<Source> {
      @Resource
      protected WebServiceContext context;
      public Source invoke(Source source) { ... } }
    public class HelloClient {
      @WebServiceRef(wsdlLocation="...?wsdl")
      HelloService service;
      ...
        Dispatch<Source> d =             // SOAPMessage
          service.createDispatch(portQName, Source.class,
                                 Service.Mode.PAYLOAD);
        Map<String, Object> c = d.getRequest/ResponseContext();
        Source result = d.invoke(...); // .invokeAsync

## `Dispatch` / `Provider` PROS/CONS

PROS

-   access to XML

-   `Dispatch` has async

-   access to context

CONS

-   no `Provider` async

-   no response context access in `Provider`

-   `Dispatch` / `BindingProvider` response context decoupled from `AsyncHandler`

-   no thread guarantees

-   cannot "own" transport

-   only `Source` and `SOAPMessage` supported
    
    -   want `InputStream` too

# Dynamic Invocation and Service Interfaces (DISI)



## DISI client side

![](../images/presentations/2013-09-15-JavaOne/disi-client-flow.png)

## DISI service side

![](../images/presentations/2013-09-15-JavaOne/disi-service-flow.png)

## DISI responsibilities

![](../images/presentations/2013-09-15-JavaOne/disi-responsibilities.png)

# WebSockets transport



## simple WebSockets transport, subprotocol, mgmt


![](../images/presentations/2013-09-15-JavaOne/websockets-transport.png)

## WebSockets code

1.  demo

    Using [JSR-356](http://jcp.org/en/jsr/detail?id=356) : Java\texttrademark API for WebSocket

## MS-SWSB : SOAP Over WebSocket Protocol Binding Specification

    <wsdl:definitions ...> ...
     <wsdl:binding name="MyBinding" type="MyPortType"> ...
      <soap12:binding
       transport="http://schemas.microsoft.com/soap/websocket"/>
       <wsdl:operation name="MyOp"> ... </wsdl:operation>
     </wsdl:binding>
     <wsdl:service name="MyService">
      <wsdl:port name="MyPort" binding="MyBinding">
       <soap12:address location=" ws://myHost/myService/" />
      </wsdl:port>
     </wsdl:service>
    </wsdl:definitions>

## MS-SWSB : SOAP/WebSocket protocol example

    GET http://myHost/myService HTTP/1.1
    Connection: Upgrade,Keep-Alive
    Upgrade: websocket
    Sec-WebSocket-Key: ROOw9dYOJkStW2nx5r1k9w==
    Sec-WebSocket-Version: 13
    Sec-WebSocket-Protocol: soap
    soap-content-type: application/soap+msbinsession1
    microsoft-binary-transfer-mode: Buffered
    Accept-Encoding: gzip, deflate

---

    HTTP/1.1 101 Switching Protocols
    Upgrade: websocket
    Connection: Upgrade
    Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
    Sec-WebSocket-Protocol: soap

# InfiniBand transport



## why InfiniBand

![](../images/presentations/2013-09-15-JavaOne/ib-why.png)

## InfiniBand communications model

![](../images/presentations/2013-09-15-JavaOne/ib-communications-model.png)

## InfiniBand queue-pair model

![](../images/presentations/2013-09-15-JavaOne/ib-qp-model.png)

## InfiniBand usage

![](../images/presentations/2013-09-15-JavaOne/ib-app-flow.png)

## InfiniBand code

1.  demo

    Using proprietary Java\texttrademark APIs on top of proprietary C APIs.

## InfiniBand credits

InfiniBand slides taken from

-   Vadim Makhervaks (Oracle)

-   <http://www.ics.uci.edu/~ccgrid11/files/ccgrid11-ib-hse_last.pdf>

# DISI major interfaces



## DISI client factories

![](../images/presentations/2013-09-15-JavaOne/disi-client-creation.png)

## DISI service factories

![](../images/presentations/2013-09-15-JavaOne/disi-service-creation.png)

## DISI client request (+ MessageContext factory)

![](../images/presentations/2013-09-15-JavaOne/disi-client-req.png)

## DISI service request (+ MessageContext factory)

![](../images/presentations/2013-09-15-JavaOne/disi-service-req.png)

## DISI client response (+ MessageContext factory)

![](../images/presentations/2013-09-15-JavaOne/disi-client-resp.png)

## DISI service response (+ MessageContext factory)

![](../images/presentations/2013-09-15-JavaOne/disi-service-resp.png)

## DISI `MessageContext` / `Factory`

    public interface MessageContextFactory {
      MessageContext createContext();
      MessageContext createContext(SOAPMessage m);
      MessageContext createContext(Source m);
      MessageContext createContext(Source m,O EnvelopeStyle.Style envelopeStyle);
      MessageContext createContext(InputStream in,
                                   String contentType) throws IOException;
    }
    public interface MessageContext {
        SOAPMessage getAsSOAPMessage() throws SOAPException;
        SOAPMessage getAsSource();
        ContentType writeTo(OutputStream out) throws IOException;
        ContentType getContentType();
        Object get(Object k);
        void   put(Object k, Object v);
    }

# summary

## summary


![](../images/presentations/2013-09-15-JavaOne/summary.png)


---

table of contents (non-functional)

<div id="table-of-contents">
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. me, disclaimer, copyright</a>
<ul>
<li><a href="#sec-1-1">1.1. BOF7479 : SOAP over WebSockets and InfiniBand with JAX-WS Pluggable Transports</a></li>
<li><a href="#sec-1-2">1.2. disclaimer, copyright</a></li>
</ul>
</li>
<li><a href="#sec-2">2. intro</a>
<ul>
<li><a href="#sec-2-1">2.1. what you will learn</a></li>
<li><a href="#sec-2-2">2.2. motivation: SOAP passing thru intermediary</a></li>
<li><a href="#sec-2-3">2.3. JAX-WS POJO usage</a></li>
<li><a href="#sec-2-4">2.4. JAX-WS XML usage</a></li>
<li><a href="#sec-2-5">2.5. JAX-WS standard includes/lacks</a></li>
<li><a href="#sec-2-6">2.6. proposal</a></li>
<li><a href="#sec-2-7">2.7. benefits</a></li>
</ul>
</li>
<li><a href="#sec-3">3. programming with existing JAX-WS</a>
<ul>
<li><a href="#sec-3-1">3.1. POJO example</a></li>
<li><a href="#sec-3-2">3.2. POJO PROS/CONS</a></li>
<li><a href="#sec-3-3">3.3. <code>Dispatch</code> / <code>Provider</code> example</a></li>
<li><a href="#sec-3-4">3.4. <code>Dispatch</code> / <code>Provider</code> PROS/CONS</a></li>
</ul>
</li>
<li><a href="#sec-4">4. Dynamic Invocation and Service Interfaces (DISI)</a>
<ul>
<li><a href="#sec-4-1">4.1. DISI client side</a></li>
<li><a href="#sec-4-2">4.2. DISI service side</a></li>
<li><a href="#sec-4-3">4.3. DISI responsibilities</a></li>
</ul>
</li>
<li><a href="#sec-5">5. WebSockets transport</a>
<ul>
<li><a href="#sec-5-1">5.1. simple WebSockets transport, subprotocol, mgmt</a></li>
<li><a href="#sec-5-2">5.2. WebSockets code</a></li>
<li><a href="#sec-5-3">5.3. MS-SWSB : SOAP Over WebSocket Protocol Binding Specification</a></li>
<li><a href="#sec-5-4">5.4. MS-SWSB : SOAP/WebSocket protocol example</a></li>
</ul>
</li>
<li><a href="#sec-6">6. InfiniBand transport</a>
<ul>
<li><a href="#sec-6-1">6.1. why InfiniBand</a></li>
<li><a href="#sec-6-2">6.2. InfiniBand communications model</a></li>
<li><a href="#sec-6-3">6.3. InfiniBand queue-pair model</a></li>
<li><a href="#sec-6-4">6.4. InfiniBand usage</a></li>
<li><a href="#sec-6-5">6.5. InfiniBand code</a></li>
<li><a href="#sec-6-6">6.6. InfiniBand credits</a></li>
</ul>
</li>
<li><a href="#sec-7">7. DISI major interfaces</a>
<ul>
<li><a href="#sec-7-1">7.1. DISI client factories</a></li>
<li><a href="#sec-7-2">7.2. DISI service factories</a></li>
<li><a href="#sec-7-3">7.3. DISI client request (+ MessageContext factory)</a></li>
<li><a href="#sec-7-4">7.4. DISI service request (+ MessageContext factory)</a></li>
<li><a href="#sec-7-5">7.5. DISI client response (+ MessageContext factory)</a></li>
<li><a href="#sec-7-6">7.6. DISI service response (+ MessageContext factory)</a></li>
<li><a href="#sec-7-7">7.7. DISI <code>MessageContext</code> / <code>Factory</code></a></li>
</ul>
</li>
<li><a href="#sec-8">8. summary</a>
<ul>
<li><a href="#sec-8-1">8.1. summary</a></li>
</ul>
</li>
</ul>
</div>
</div>
