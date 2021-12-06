%%%% -*- Mode: Prolog -*-
%%%% Diego Vismara 844796


uri_parse(URIString, URI) :-

	URI = uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment),

	string_to_atom(URIString, URIAtom),
	atom_chars(URIAtom, URIChars),

	scheme(URIChars, Scheme1, Userinfo1, Host1, Port1, Path1, Query1, Fragment1),

	build(Scheme1, Scheme),
	build(Userinfo1, Userinfo),
	build(Host1, Host),
	build(Port1, Port),
	build(Path1, Path),
	build(Query1, Query),
	build(Fragment1, Fragment).

% Predicato che converte la stringa in output.
% In caso di stringa vuota non viene convertita
build([], []) :- !.
build(List, Result) :-
	atom_chars(List1, List),
	string_to_atom(Result, List1).


% INIZIO CONTROLLO SCHEME

% Check scheme, se uguale a 'mailto'
% si passa al controllo specifico 'scheme_mailto'
scheme([C1, C2, C3, C4, C5, C6, C7 | Cs],
       [C1, C2, C3, C4, C5, C6],
       Userinfo,
       Host,
       [], [], [], []) :-
	C1 == 'm',
	C2 == 'a',
	C3 == 'i',
	C4 == 'l',
	C5 == 't',
	C6 == 'o',
	C7 == ':',
	!,
	scheme_mailto(Cs, Userinfo, Host).

% Check scheme, se uguale a 'news'
% si passa al controllo specifico 'scheme_news'
scheme([C1, C2, C3, C4, C5 | Cs],
       [C1, C2, C3, C4],
       [],
       Host,
       [], [], [], []) :-
	C1 == 'n',
	C2 == 'e',
	C3 == 'w',
	C4 == 's',
	C5 == ':',
	!,
	scheme_news(Cs, Host).

% Check scheme, se uguale a 'tel'
% si passa al controllo specifico 'scheme_tel_fax'
scheme([C1, C2, C3, C4 | Cs],
       [C1, C2, C3],
       Userinfo,
       [], [], [], [], []) :-
	C1 == 't',
	C2 == 'e',
	C3 == 'l',
	C4 == ':',
	!,
	scheme_tel_fax(Cs, Userinfo).

% Check scheme, se uguale a 'fax'
% si passa al controllo specifico 'scheme_tel_fax'
scheme([C1, C2, C3, C4 | Cs],
       [C1, C2, C3],
       Userinfo,
       [], [], [], [], []) :-
	C1 == 'f',
	C2 == 'a',
	C3 == 'x',
	C4 == ':',
	!,
	scheme_tel_fax(Cs, Userinfo).


% NON VA!
% In caso di http/https senza porta, viene assegnata
% la 80 di default
%scheme([C1, C2, C3, C4, C5 | Cs],
%       [C1, C2, C3, C4],
%       Userinfo, Host, [], Path, Query, Fragment) :-
%	C1 == 'h',
%	C2 == 't',
%	C3 == 't',
%	C4 == 'p',
%	C5 == ':',
%	%Port == [],
%	!,
%	Port = 80,
%	%id(Cs, Cs1, Scheme),
%	%print(Port),
%	scheme_2(Cs, Userinfo, Host, Port, Path, Query, Fragment).

%scheme([C1, C2, C3, C4, C5, C6 | Cs],
%       [C1, C2, C3, C4, C5],
%       Userinfo, Host, [], Path, Query, Fragment) :-
%	C1 == 'h',
%	C2 == 't',
%	C3 == 't',
%	C4 == 'p',
%	C5 == 's',
%	C6 == ':',
%	%Port == [],
%	!,
%	Port = 80,
%	%id(Cs, Cs1, Scheme),
%	%print(Port),
%	scheme_2(Cs, Userinfo, Host, Port, Path, Query, Fragment).



% Nel caso in cui non coincida con nessuno scheme specifico
% si effetua il check che lo scheme sia un ID valido
% e si passa al controllo 'scheme_2'
scheme(Cs, Scheme, Userinfo, Host, Port, Path, Query, Fragment) :-
	id(Cs, Cs1, Scheme),
	scheme_2(Cs1, Userinfo, Host, Port, Path, Query, Fragment).

% Check presenza ':' dopo scheme
scheme_2([C | Cs], Userinfo, Host, Port, Path, Query, Fragment) :-
	C == ':',
	scheme_3(Cs, Userinfo, Host, Port, Path, Query, Fragment).

% In caso di doppio '//' successvo a ':'
% si passa a 'caso_1' (con authority)
scheme_3([C1, C2 | Cs], Userinfo, Host, Port, Path, Query, Fragment) :-
	C1 == '/',
	C2 == '/',
	!,
	caso_1(Cs, Userinfo, Host, Port, Path, Query, Fragment).

% Se fallisce con authority, si passa a 'caso_2'
scheme_3(Cs, Userinfo, Host, Port, Path, Query, Fragment) :-
	Userinfo = [],
	Host = [],
	Port = [],
	caso_2(Cs, Path, Query, Fragment).

% Casi specifici per gli scheme: mailto - news - telfax
scheme_mailto(Cs, Userinfo, Host) :-
	id(Cs, Cs1, Userinfo),
	scheme_mailto_2(Cs1, Host).

scheme_mailto_2([], []) :- !.
scheme_mailto_2([C | Cs], Host) :-
	C == '@',
	host(Cs, [], Host).

scheme_news(Cs, Host) :-
	host(Cs, [], Host).

scheme_tel_fax(Cs, Userinfo) :-
	id(Cs, [], Userinfo).

% FINE CONTROLLO SCHEME

% INIZIO CASO CON AUTHORITHY
% Check 'caso_1' (con authority).
% Si divide in due predicati: il primo controlla che ci sia la userinfo,
% nel caso non sia presente, fallisce e passa il controllo a quello
% senza userinfo.
caso_1(Cs, Userinfo, Host, Port, Path, Query, Fragment) :-
	id(Cs, Cs1, Userinfo),
	caso_userinfo(Cs1, Host, Port, Path, Query, Fragment),
	!.
caso_1(Cs, [], Host, Port, Path, Query, Fragment) :-
	caso_nouserinfo(Cs, Host, Port, Path, Query, Fragment).

% Se e' presente '@' allora ci si trova nel caso con userinfo e
% si prosegue a controllare l'host.
caso_userinfo([C | Cs], Host, Port, Path, Query, Fragment) :-
	C == '@',
	!,
	caso_nouserinfo(Cs, Host, Port, Path, Query, Fragment).

% Il primo predicato 'caso_nouserinfo' controlla che sia presente
% l'host.
caso_nouserinfo(Cs, Host, Port, Path, Query, Fragment) :-
	host(Cs, Cs1, Host),
	!,
	caso_nouserinfo_2(Cs1, Port, Path, Query, Fragment).

% In questo predicato viene controllato che sia presente port
% precenuta da ':'
caso_nouserinfo_2([C | Cs], Port, Path, Query, Fragment) :-
	C == ':',
	!,
	port(Cs, Cs1, Port),
	caso_nouserinfo_3(Cs1, Path, Query, Fragment).

%Nel caso non sia presente port si passa a 'caso_nouserinfo_3'
caso_nouserinfo_2(Cs, [], Path, Query, Fragment) :-
	caso_nouserinfo_3(Cs, Path, Query, Fragment).

% Se e' presente '/', si controlla path, query e fragment (facoltativi)
caso_nouserinfo_3([], [], [], []) :- !.
caso_nouserinfo_3([C | Cs], Path, Query, Fragment) :-
	C == '/',
	!,
	path(Cs, Path, Query, Fragment).

% FINE CASO CON AUTHORITHY

% INIZIO CASO SENZA AUTHORITHY
% Nel caso in cui 'caso_1' fallisca, il controllo passa a 'caso_2'.
% Facoltativa la presenza di '/', path, query e fragment .
caso_2([C | Cs], Path, Query, Fragment) :-
	C == '/',
	!,
	path(Cs, Path, Query, Fragment).
caso_2(Cs, Path, Query, Fragment) :-
	path(Cs, Path, Query, Fragment).

% FINE CASO SENZA AUTHORITHY

% Un path e' la concatenazione di almeno un ID con altri ID, separati da
% '/'. Successivamente a '/' deve essere presente un ID. In caso ci sia
% '?' si passa al controllo query, altrimenti si passa al fragment e la
% query rimarra' vuota
path(Cs, Path, Query, Fragment) :-
	id(Cs, Cs1, Path1),
	!,
	path_2(Cs1, Path2, Query, Fragment),
	merge_list(Path1, Path2, Path).
path(Cs, Path, Query, Fragment) :-
	Path = [],
	query(Cs, Query, Fragment).

path_2([C | Cs], Path, Query, Fragment) :-
	C == '/',
	id(Cs, Cs1, Path1),
	!,
	path_2(Cs1, Path2, Query, Fragment),
	merge_list([C | Path1], Path2, Path).
path_2(Cs, [], Query, Fragment) :-
	query(Cs, Query, Fragment),
	!.

% Nel caso sia presente '?' si verifica che sia presente una
% query; ovvero la concatenazione di qualsiasi carattere diverso da '#',
% Nel caso in cui vienga riconosciuto '#', si passa al
% controllo per il fragment.
query([C1, C2 | Cs], [C2 | Query], Fragment) :-
	C1 == '?',
	C2 \= '#',
	!,
	query_2(Cs, Query, Fragment).
query([C | Cs], [], Fragment) :-
	C \= '?',
	fragment(Cs, Fragment).

query_2([C | Cs], [C | Query], Fragment) :-
	C \= '#',
	!,
	query_2(Cs, Query, Fragment).
query_2(Cs, [], Fragment) :-
	fragment(Cs, Fragment).

% Nel caso sia presente '#' allora si verifica che sia presente un
% fragment; ovvero la concatenazione di qualsiasi carattere, con almeno
% un carattere
fragment([], []) :- !.
fragment([C | Cs], Fragment) :-
	C == '#',
	fragment_2(Cs, Fragment).

fragment_2([C | Cs], [C | Fragment]) :-
	fragment_3(Cs, Fragment).

fragment_3([], []) :- !.
fragment_3([C | Cs], [C | Fragment]) :-
	fragment_3(Cs, Fragment).

% Per ID si intende una stringa che non contiene questi caratteri:
% '/', '?',  '#',  '@',	':' la stringa vuota non viene riconosciuta
id([C | Cs], Cs1, [C | Is]) :-
	C \= '/',
	C \= '?',
	C \= '#',
	C \= '@',
	C \= ':',
	!,
	id_2(Cs, Cs1, Is).

id_2([C | Cs], Cs1, [C | Is]) :-
	C \= '/',
	C \= '?',
	C \= '#',
	C \= '@',
	C \= ':',
	!,
	id_2(Cs, Cs1, Is).
id_2(Cs, Cs, []).

% Per host si intende la concatenazione di diversi 'host_id'.
% Il predicato 'merge_list' viene usato per concatenare i vari risultati
% ricorsivamente
host(Cs, Cs1, Host) :-
	indirizzo_ip(Cs, Cs1, Host),
	!.
host(Cs, Cs1, Host) :-
	host_id(Cs, Cs2, Host1),
	!,
	host_2(Cs2, Cs1, Host2),
	merge_list(Host1, Host2, Host).
host_2([], [], []) :- !.

host_2([C | Cs], Cs1, Host) :-
	C == '.',
	!,
	host_id(Cs, Cs2, Host1),
	host_2(Cs2, Cs1, Host2),
	merge_list([C | Host1], Host2, Host).
host_2(Cs, Cs, []).

% Per 'host_id' si intende una stringa che non contiene questi
% caratteri: '.', '/', '?', '#', '@', ':'.
% La stringa vuota non viene riconosciuta
host_id([C | Cs], Cs1, [C | Is]) :-
	C \= '.',
	C \= '/',
	C \= '?',
	C \= '#',
	C \= '@',
	C \= ':',
	!,
	host_id_2(Cs, Cs1, Is).

host_id_2([], [], []) :- !.
host_id_2([C | Cs], Cs1, [C | Is]) :-
	C \= '.',
	C \= '/',
	C \= '?',
	C \= '#',
	C \= '@',
	C \= ':',
	!,
	host_id_2(Cs, Cs1, Is).
host_id_2(Cs, Cs, []).

% Riconosce una porta; ovvero una concatenazione di digit.
% Non viene riconosciuta la stringa vuota
port([C | Cs], Cs1, [C | Port]) :-
	digit(C),
	!,
	port_2(Cs, Cs1, Port).

port_2([C | Cs], Cs1, [C | Port]) :-
	digit(C),
	!,
	port_2(Cs, Cs1, Port).
port_2(Cs, Cs, []).

% Riconosce un indirizzo IP; composto da quattro gruppi di tre digit
% divisi da un '.'
indirizzo_ip([C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15
	     | Cs],
	     Cs,
	     [C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]) :-
	digit(C1),
	digit(C2),
	digit(C3),
	C4 == '.',
	digit(C5),
	digit(C6),
	digit(C7),
	C8 == '.',
	digit(C9),
	digit(C10),
	digit(C11),
	C12 == '.',
	digit(C13),
	digit(C14),
	digit(C15).

% Riconosce se C e' una cifra.
digit(C) :-
	C == '0';
	C == '1';
	C == '2';
	C == '3';
	C == '4';
	C == '5';
	C == '6';
	C == '7';
	C == '8';
	C == '9'.

% Predicati utili a concatenare i risultati dell'host
merge_list([], [], []) :- !.
merge_list([], Bs, Bs) :- !.
merge_list(As, [], As) :- !.
merge_list([A | As], Bs, [A | Cs]) :-
	merge_list(As, Bs, Cs).

% Predicati "utilities" per stampare in modo testuale
% un URI

% uri_display/1 string
% Riceve in input una stringa contenente l'URI
% e stampa ogni suo componente
uri_display("") :- !.
uri_display(URIString) :-
	uri_display(URIString, _).

% uri_display/2 string URI
% Variante del predicato 'uri_parse' in cui l'URI
% viene scomposto in ogni suo componente per comporre una lista.
% Viene stampata sia la lista di tutti i componenti,
% sia l'URI in formato testuale
uri_display(URIString, URIStamp) :-

	URIStamp = [Scheme, Userinfo, Host, Port, Path, Query, Fragment],

	string_to_atom(URIString, URIAtom),
	atom_chars(URIAtom, URIChars),

	scheme(URIChars, Scheme1, Userinfo1, Host1, Port1, Path1, Query1, Fragment1),

	build(Scheme1, Scheme),
	build(Userinfo1, Userinfo),
	build(Host1, Host),
	build(Port1, Port),
	build(Path1, Path),
	build(Query1, Query),
	build(Fragment1, Fragment),
	uri_display(URIStamp, 0).

% uri_display/2 List Index
% Riceve in input una lista di componenti URI e la stampa testualmente
% anteponendo al componente il suo nome identificativo.
% L'Index serve per creare una sorta di "switch case"
uri_display([], _) :- !.
uri_display([A|B], N) :-

	N == 0 -> string_upper(A, U),
	write("Scheme: "),
	writeln(U),
	N1 is N + 1,
	uri_display(B, N1);

	N == 1 -> write("Userinfo: "),
	print(A),
	writeln(""),
	N1 is N + 1,
	uri_display(B, N1);

	N == 2 -> write("Host: "),
	print(A),
	writeln(""),
	N1 is N + 1,
	uri_display(B, N1);

	N == 3 -> write("Port: "),
	writeln(A),
	N1 is N + 1,
	uri_display(B, N1);

	N == 4 -> write("Path: "),
	print(A),
	writeln(""),
	N1 is N + 1,
	uri_display(B, N1);

	N == 5 -> write("Query: "),
	print(A),
	writeln(""),
	N1 is N + 1,
	uri_display(B, N1);

	N == 6 -> write("Fragment: "),
	print(A),
	writeln(""),
	writeln(""),
	N1 is N + 1,
	uri_display(B, N1).

%%%% end of file -- uri-parse
