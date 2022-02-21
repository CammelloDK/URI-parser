LP_E1P_2022: URI parser


URI_PARSE/2 URIString URI
Scompone la stringa nei 7 campi principali di cui è composto un URI:
Scheme, UserInfo, Host, Port (viene assegnata la 80 di default in caso non venga specificata), Path, Query, Fragment

Esempi di chiamata:
	uri_parse("http://disco.unimib.it", URI).
	URI = uri(http, [], 'disco.unimib.it', 80, [], [], []).

	uri_parse("telnet://192.168.0.3:267", URI).
	URI = uri(telnet, [], '192.168.0.3', 267, [], [], []).

	uri_parse("telnet://192.168.0..3:267", URI).
	false.

	uri_parse("http://it.wikipedia.org/application/new_user/registration_form?nome=Mario&cognome=Rossi&ID_utente=M_Rossi", URI).
	URI = uri(http, [], 'it.wikipedia.org', 80, 'application/new_user/registration_form', 'nome=Mario&cognome=Rossi&ID_utente=M_Rossi', []).

	uri_parse("https://example.com:443/data.csv/", URI).
	URI = uri(https, [], 'example.com', 443, 'data.csv', [], []).

	uri_parse("mailto:someone@example.com", URI).
	URI = uri(mailto, someone, 'example.com', 80, [], [], []).

	uri_parse("mailto:someoneexample.com", URI).
	false.

	uri_parse("news:comp.infosystems.www.servers.unix", URI).
	URI = uri(news, [], 'comp.infosystems.www.servers.unix', 80, [], [], []).

	uri_parse("news:comp.infosystems.www.servers.unix:24/weeklynews", URI).
	false.

	uri_parse("fax:+391234567890", URI).
	URI = uri(fax, '+391234567890', [], 80, [], [], []).

	uri_parse("ftp://ftp.is.co.za/rfc/rfc1808.txt/", URI).
	URI = uri(ftp, [], 'ftp.is.co.za', 80, 'rfc/rfc1808.txt', [], []).
	
	uri_parse("zos://me@hercules.disco.unimib.it:372/myproj.linalg.fortran(svd)?submit=FORTXC#ID=7", URI).
	URI = uri(zos, me, 'hercules.disco.unimib.it', 372, 'myproj.linalg.fortran(svd)', 'submit=FORTXC', 'ID=7').

	uri_parse("http://disco.unimib.it", uri(http, _, _, _, _, _, _)).
	true.

	uri_parse("http://disco.unimib.it", uri(https, _, _, _, _, _, _)).
	false.

	uri_parse("http://disco.unimib.it", uri(http, _, _, 80, _, _, _)).
	true.

	uri_parse("mailto:someone@example.com", uri(_, Userinfo, Host, _, _, _, _)).
	Userinfo = someone,
	Host = 'example.com'.

	uri_parse("telnet://192.168.0.3:267", uri(_, _, _, _, _, _, Fragment)).
	Fragment = [].

	uri_parse("zos://me@hercules.disco.unimib.it:372/myproj.linalg.fortran(svd)?submit=FORTXC#ID=7", uri(_, _, _, _, 'myproj.linalg.fortran(svd)', _, 'ID=7')).
	true.
	
_______________________________

URI_DISPLAY/1 URI
Stampa tutte le componenti dell'URI.

Esempi di chiamata:

       uri_parse("http://disco.unimib.it", URI),
       |    uri_display(URI).
       Scheme: HTTP
       Userinfo: []
       Host: 'disco.unimib.it'
       Port: 80
       Path: []
       Query: []
       Fragment: []

       URI = uri(http, [], 'disco.unimib.it', 80, [], [], []).


       uri_parse("zos://me@hercules.disco.unimib.it:372/myproj.linalg.fortran(svd)?submit=FORTXC#ID=7", URI),
       |    uri_display(URI).
       Scheme: ZOS
       Userinfo: 'me'
       Host: 'hercules.disco.unimib.it'
       Port: 372
       Path: 'myproj.linalg.fortran(svd)'
       Query: 'submit=FORTXC'
       Fragment: 'ID=7'

       URI = uri(zos, me, 'hercules.disco.unimib.it', 372, 'myproj.linalg.fortran(svd)', 'submit=FORTXC', 'ID=7').

_______________________________

URI_DISPLAY/2 URI Stream
Stampa sullo stream tutte le componenti dell'URI.
Eg. stampa su file

Esempi di chiamata:
       uri_parse("http://disco.unimib.it", URI),
       |    open('outputfile.txt', append, Stream),
       |    uri_display(URI, Stream).
       URI = uri(http, [], 'disco.unimib.it', 80, [], [], []),
       Stream = <stream>(0x600002314600).

       outputfile.txt
	Scheme: HTTP
       	Userinfo: []
       	Host: 'disco.unimib.it'
       	Port: 80
       	Path: []
       	Query: []
       	Fragment: []
       

       uri_parse("zos://me@hercules.disco.unimib.it:372/myproj.linalg.fortran(svd)?submit=FORTXC#ID=7", URI),
       |    open('outputfile.txt', append, Stream),
       |    uri_display(URI, Stream).
       URI = uri(zos, me, 'hercules.disco.unimib.it', 372, 'myproj.linalg.fortran(svd)', 'submit=FORTXC', 'ID=7'),
       Stream = <stream>(0x600002352400).

       outputfile.txt
	Scheme: HTTP
       	Userinfo: []
       	Host: 'disco.unimib.it'
       	Port: 80
       	Path: []
       	Query: []
       	Fragment: []
       	
       	Scheme: ZOS
       	Userinfo: 'me'
       	Host: 'hercules.disco.unimib.it'
       	Port: 372
       	Path: 'myproj.linalg.fortran(svd)'
       	Query: 'submit=FORTXC'
       	Fragment: 'ID=7'
       
