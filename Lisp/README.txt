LP_E1P_2022: URI parser

Componenti del gruppo:
844796 Diego Vismara


URI-PARSE string -> uri-structure
Scompone la stringa nei 7 campi principali di cui è composto un URI:
Scheme, UserInfo, Host, Port (viene assegnata la 80 di default in caso non venga specificata), Path, Query, Fragment

Esempi di chiamata:
	(uri-parse "http://disco.unimib.it")
	#S(URI :SCHEME "http" :USERINFO NIL :HOST "disco.unimib.it" :PORT "80" :PATH NIL :QUERY NIL :FRAGMENT NIL)

	(uri-parse "telnet://192.168.0.3:267/")
	#S(URI :SCHEME "telnet" :USERINFO NIL :HOST "192.168.0.3" :PORT "267" :PATH NIL :QUERY NIL :FRAGMENT NIL)

	(uri-parse "telnet://192.168.0..3:267/")
	Error: NOT VALID HOST: HOST IS OBLIGATORY
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}

	(uri-parse "http://it.wikipedia.org/application/new_user/registration_form?nome=Mario&cognome=Rossi&ID_utente=M_Rossi")
	#S(URI :SCHEME "http" :USERINFO NIL :HOST "it.wikipedia.org" :PORT "80" :PATH "application/new_user/registration_form" :QUERY "nome=Mario&cognome=Rossi&ID_utente=M_Rossi" :FRAGMENT NIL)

	(uri-parse "https://example.com:443/data.csv#row=4")
	#S(URI :SCHEME "https" :USERINFO NIL :HOST "example.com" :PORT "443" :PATH "data.csv" :QUERY NIL :FRAGMENT "row=4")

	(uri-parse "mailto:someone@example.com")
	#S(URI :SCHEME "mailto" :USERINFO "someone" :HOST "example.com" :PORT "80" :PATH NIL :QUERY NIL :FRAGMENT NIL)

	(uri-parse "mailto:someoneexample.com")
	Error: NOT VALID USERINFO: USERINFO IS OBLIGATORY
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}

	(uri-parse "mailto:someone@example.com:31")
	Error: NOT VALID HOST
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}

	(uri-parse "news:comp.infosystems.www.servers.unix")
	#S(URI :SCHEME "news" :USERINFO NIL :HOST "comp.infosystems.www.servers.unix" :PORT "80" :PATH NIL :QUERY NIL :FRAGMENT NIL)

	(uri-parse "news:comp.infosystems.www.servers.unix:24/weeklynews")
	Error: NOT VALID HOST: HOST IS OBLIGATORY
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}
	

	(uri-parse "fax:+391234567890")
	#S(URI :SCHEME "fax" :USERINFO "+391234567890" :HOST NIL :PORT "80" :PATH NIL :QUERY NIL :FRAGMENT NIL)

	(uri-parse "ftp://ftp.is.co.za/rfc/rfc1808.txt")
	#S(URI :SCHEME "ftp" :USERINFO NIL :HOST "ftp.is.co.za" :PORT "80" :PATH "rfc/rfc1808.txt" :QUERY NIL :FRAGMENT NIL)
	
	(uri-parse "zos://me@hercules.disco.unimib.it:372/myproj.linalg.fortran(svd)?submit=FORTXC#ID=7")
	#S(URI :SCHEME "zos" :USERINFO "me" :HOST "hercules.disco.unimib.it" :PORT "372" :PATH "myproj.linalg.fortran(svd)" :QUERY "submit=FORTXC" :FRAGMENT "ID=7")

_______________________________

URI-SCHEME uri-structure -> string
Stampa lo scheme dell'URI. Insieme alla porta è sempre presente.

Esempi di chiamata:
       (uri-scheme (uri-parse "http://disco.unimib.it"))
       "http"

       (uri-parse "mailto:someone@example.com")
       "mailto"

_______________________________

URI-USERINFO uri-structure -> string
Stampa lo userinfo dell'URI se presente, NIL altrimenti

Esempi di chiamata:
       (uri-userinfo (uri-parse "http://disco.unimib.it"))
       NIL

       (uri-userinfo (uri-parse "mailto:someone@example.com"))
       "someone"

_______________________________

URI-HOST uri-structure -> string
Stampa l'host dell'URI se presente, NIL altrimenti

Esempi di chiamata:
       (uri-host (uri-parse "http://disco.unimib.it"))
       "disco.unimib.it"

       (uri-host (uri-parse "mailto:someone@example.com"))
       "example.com"

       (uri-host (uri-parse "tel:+391234567890"))
       NIL

_______________________________

URI-PATH uri-structure -> string
Stampa il path dell'URI se presente, NIL altrimenti

Esempi di chiamata:
       (uri-path (uri-parse "http://disco.unimib.it"))
       NIL

       (uri-path (uri-parse "https://example.com:443/path/to/file/file.csv#row=4"))
       "path/to/file/file.csv"

_______________________________

URI-QUERY uri-structure -> string
Stampa la query dell'URI se presente, NIL altrimenti

Esempi di chiamata:
       (uri-query (uri-parse "http://disco.unimib.it"))
       NIL

       (uri-query (uri-parse "http://it.wikipedia.org/application/new_user/registration_form?nome=Mario&cognome=Rossi&ID_utente=M_Rossi"))
       "nome=Mario&cognome=Rossi&ID_utente=M_Rossi"

_______________________________

URI-FRAGMENT uri-structure -> string
Stampa il fragment dell'URI se presente, NIL altrimenti

Esempi di chiamata:
       (uri-fragment (uri-parse "http://disco.unimib.it"))
       NIL

       (uri-fragment (uri-parse "https://example.com:443/data.csv#row=4"))
       "row=4"

_______________________________

URI-DISPLAY uri-structure &optional stream -> T
Stampa sullo stream tutte le componenti dell'URI. Di default stampa a terminale, test riuscito anche su file

Esempi di chiamata:

       STANDARD OUTPUT
       
       (uri-display (uri-parse "http://disco.unimib.it"))
       Scheme:   HTTP
       Userinfo: NIL
       Host:     "disco.unimib.it"
       Port:     80
       Path:     NIL
       Query:    NIL
       Fragment: NIL
       
       T

       (uri-display (uri-parse "zos://me@hercules.disco.unimib.it:372/myproj.linalg.fortran(svd)?submit=FORTXC#ID=7"))
       Scheme:   ZOS
       Userinfo: me
       Host:     "hercules.disco.unimib.it"
       Port:     372
       Path:     "myproj.linalg.fortran(svd)"
       Query:    "submit=FORTXC"
       Fragment: "ID=7"

       T

       FILE OUTPUT
       
       (with-open-file
	(stream "/path/to/file/Vismara_Diego_844796_LP_E1P_2022/Lisp/outputfile.txt" :if-does-not-exist :create :if-exists :append :direction :output)
  	(uri-display (uri-parse "http://disco.unimib.it") stream))

       T

       outputfile.txt
	Scheme:   HTTP
       	Userinfo: NIL
       	Host:     "disco.unimib.it"
       	Port:     80
       	Path:     NIL
       	Query:    NIL
       	Fragment: NIL
       	
	
       (with-open-file
	(stream "/path/to/file/Vismara_Diego_844796_LP_E1P_2022/Lisp/outputfile.txt" :if-does-not-exist :create :if-exists :append :direction :output)
  	(uri-display (uri-parse "zos://me@hercules.disco.unimib.it:372/myproj.linalg.fortran(svd)?submit=FORTXC#ID=7") stream))

       T

       outputfile.txt
	Scheme:   HTTP
       	Userinfo: NIL
       	Host:     "disco.unimib.it"
       	Port:     80
       	Path:     NIL
       	Query:    NIL
       	Fragment: NIL
       	
	Scheme:   ZOS
       	Userinfo: me
       	Host:     "hercules.disco.unimib.it"
       	Port:     372
       	Path:     "myproj.linalg.fortran(svd)"
       	Query:    "submit=FORTXC"
       	Fragment: "ID=7"
	
