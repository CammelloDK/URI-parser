;;;; -*- Mode: Lisp -*-
;;;; Diego Vismara 844796

;; Definizione struttura
;; :scheme :userinfo :host :port :path :query :fragment
(defstruct uri scheme userinfo host port path query fragment)

;; Funzione principale
;; uri-parse:  string --> list
(defun uri-parse (string)
  (let ((list (coerce-to-list string)))
    (if (null list) (error "Empty is not a URI")
      (check-scheme list))))

;; Queste funzioni definiscono i vari elementi dell'URI
;; <query, fragment, host-identifier, identifier, digit>
(defun is-query (list)
  (if (null list) nil
    (eval (cons 'and (mapcar (lambda (char) (if (eq char #\#) nil t)) list)))))

(defun is-fragment (list)
  (if (null list) nil
    (eval (cons 'and (mapcar 'characterp list)))))

(defun is-char-id (char)
  (if (null char) nil
    (if (or (eq char #\/) 
            (eq char #\?)
            (eq char #\#)
            (eq char #\@)
            (eq char #\:))
        nil t)))

(defun is-char-host (char)
  (if (or (eq char #\.)
          (eq char #\/)
          (eq char #\?)
          (eq char #\#)
          (eq char #\@)
          (eq char #\:))
      nil t))

(defun is-identificatore (list)
  (eval (cons 'and (mapcar (lambda (char)
                             (if (or (eq char #\/)
                                     (eq char #\?)
                                     (eq char #\#)
                                     (eq char #\@)
                                     (eq char #\:))
                                 nil t)) 
                           list))))

(defun is-identificatore-host (list)
  (eval (cons 'and (mapcar (lambda (char)
                             (if (or (eq char #\.)
                                     (eq char #\/)
                                     (eq char #\?)
                                     (eq char #\#)
                                     (eq char #\@)
                                     (eq char #\:))
                                 nil t)) 
                           list))))

(defun is-digit (list)
  (eval (cons 'and (mapcar (lambda (num)
                             (if (null (to-number num))
                                 nil t))
                           list))))

;; Questa funzione, con relative funzioni di supporto, effettua il check
;; della struttura di un indirizzo IP
(defun is-ip (ip)
  (let ((a (split-by-point (coerce (coerce-to-string ip) 'string))))
    (let ((b (mapcar 'coerce-to-list a)))
      (if (not (eq (length b) 4)) 
	  nil
        (let ((x (first b))
              (y (second b))
              (w (third b))
              (z (fourth b)))
          (if (and (octet-length x)
                   (octet-length y)
                   (octet-length w)
                   (octet-length z))
              T nil))))))

;; Controlla che l'ottetto sia composto da 1 a 3 cifre
(defun octet-length (oct)
  (if (and (> (length oct) 0) (< (length oct) 4))
      (octet-is-number oct)
    nil))

;; Controlla che l'ottetto sia un numero
(defun octet-is-number (oct)
  (cond ((null oct) T)
	((to-number (first oct)) (octet-is-number (cdr oct)))
	((not (to-number (first oct))) nil)))

;; Definizione sintattica di scheme
(defun is-scheme (list)
  (and (is-identificatore list) (not (null list))))

;; Definizione sintattica di userinfo
(defun is-userinfo (list)
  (if (null list) nil
    (and (is-identificatore list) (not (null list)))))

;; Definizione sintattica di path
(defun is-path (list)
  (if (null list) nil
    (if (is-char-id (car list)) (is-path2 list) nil)))

(defun is-path2 (list)
  (if (null list) t
    (if (is-char-id (car list)) (is-path2 (cdr list))
      (if (eq (car list) #\/) 
          (cond ((is-char-id (car (cdr list))) (is-path2 (cdr list)))
                ((null (cdr list)) t)
                (t nil)) 
        nil))))

(defun is-path-after-auth (list)
  (if (eq (car list) #\/) t
    (if (eq (car list) #\/) (if (is-path (cdr list)) t nil) nil)))

;; Definizione sintattica di host
(defun is-host (list)
  (if (null list) nil
    (cond ((is-ip list) t)
          ((is-char-host (car list)) (is-host2 list))
          (t nil))))

(defun is-host2 (list)
  (if (null list) t
    (if (is-char-host (car list)) (is-host2 (cdr list))
      (if (eq (car list) #\.)
          (if (is-char-host (car (cdr list))) 
              (is-host2 (cdr list))
            nil)
        nil))))

;; Definizione sintattica di port
(defun is-port (list)
  (if (null list) nil
    (is-digit list)))

;; Funzione check-scheme comincia il parsing
;; e riconosce il tipo di URI
(defun check-scheme (list)
  (let ((l (list-to-sym list #\:))
        (s (sym-to-list list #\:)))
    (if (is-scheme l) 
        (cond ((equal l '(#\m #\a #\i #\l #\t #\o)) 
               (mailto-ss s (list l)))           
              ((equal l '(#\n #\e #\w #\s)) 
               (news-ss s (list l)))
              ((or (equal l '(#\t #\e #\l))
                   (equal l '(#\f #\a #\x))) 
               (telfax-ss s (list l)))
              (t (if (and (eq (car s) #\/)
                          (eq (car (cdr s)) #\/))
                     (first-uri-type s (list l)) 
                   (second-uri-type s (list l)))))
      (error "Not valid SCHEME: SCHEME IS OBLIGATORY
          definition of SCHEME:
          scheme := <identifier>
          identifier := chars without {/ ? # @ :}"))))

;; URL
(defun first-uri-type (list urielements)
  (userinfo-opt (cdr (cdr list)) urielements))

;; URL
(defun second-uri-type (list urielements)
  (if (eq (car list) #\/) 
      (path-opt (cdr list) (append urielements
                                   '(nil) 
                                   '(nil) 
                                   '(nil)))
    (path-opt list (append urielements
                           '(nil) 
                           '(nil) 
                           '(nil)))))

;; MAILTO
(defun mailto-ss (list urielements)
  (if (is-userinfo list) (uri-build (append urielements (list list)))
    (if (is-userinfo (list-to-sym list #\@)) 
        (host-opt (sym-to-list list #\@) 
                  (append urielements (list (list-to-sym list #\@))))
      (error "NOT VALID USERINFO: USERINFO IS OBLIGATORY
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}"))))

;; NEWS
(defun news-ss (list urielements)
  (if (is-host list) (uri-build (append urielements (list nil list)))
    (error "NOT VALID HOST: HOST IS OBLIGATORY
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}")))

;; TELFAX
(defun telfax-ss (list urielements)
  (if (is-userinfo list) (uri-build (append urielements (list list)))
    (error "NOT VALID USERINFO: USERINFO IS OBLIGATORY
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}")))

;; Presenza facoltativa di
;; ['@' userinfo]
(defun userinfo-opt (list urielements) 
  (let ((l (list-to-sym list #\@))
        (s (sym-to-list list #\@)))
    (if (is-userinfo l) (host-obb s (append urielements (list l)))
      (if (and (null l) (not (eq (car list) #\@)))
          (if (null s)
              (host-obb list (append urielements
                                     '(nil)))
            (host-obb s (append urielements
                                '(nil) 
                                (list l))))
                      
        (error "NOT VALID USERINFO
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}")))))

;; Presenza obbligatoria di
;; [host]
(defun host-obb (list urielements)
  (if (is-host list) (uri-build (append urielements 
                                        (list list)))
    (cond ((is-host (list-to-sym list #\:)) 
           (port-opt (sym-to-list list #\:) 
                     (append urielements 
                             (list (list-to-sym list #\:)))))
          ((is-host (list-to-sym list #\/)) 
           (path-opt (sym-to-list list #\/) 
                     (append urielements 
                             (list (list-to-sym list #\/)) 
                             '(nil)))) 
          ((is-host (list-to-sym list #\?)) 
           (query-opt (sym-to-list list #\?) 
                      (append urielements 
                              (list (list-to-sym list #\?)) 
                              '(nil) 
                              '(nil))))
          ((is-host (list-to-sym list #\#)) 
           (fragment-opt (sym-to-list list #\#) 
                         (append urielements 
                                 (list (list-to-sym list #\#)) 
                                 '(nil) 
                                 '(nil) 
                                 '(nil))))
          (t (error "NOT VALID HOST: HOST IS OBLIGATORY
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}")))))

;; Presenza facoltatova di
;; [':' port]
(defun port-opt (list urielements)
  (if (is-port list) (uri-build (append urielements 
                                        (list list)))
    (cond ((is-port (list-to-sym list #\/)) 
           (path-opt (sym-to-list list #\/) 
                     (append urielements 
                             (list (list-to-sym list #\/)))))  
          ((is-port (list-to-sym list #\?)) 
           (query-opt (sym-to-list list #\?) 
                      (append urielements 
                              (list (list-to-sym list #\?)) 
                              '(nil))))
          ((is-port (list-to-sym list #\#)) 
           (fragment-opt (sym-to-list list #\#) 
                         (append urielements 
                                 (list (list-to-sym list #\#)) 
                                 '(nil) 
                                 '(nil))))
          (t (error "NOT VALID PORT
          definition of PORT:
          port := <digit>
          digit := chars form 0 to 9")))))
 
;; Presenza facoltatova di
;; [path]
(defun path-opt (list urielements)
  (cond ((is-path list) (uri-build (append urielements 
                                           (list list))))
        ((eq (car list) #\?) (query-opt
                              (cdr list)
                              (append urielements 
                                      '(nil))))
        ((eq (car list) #\#) (fragment-opt
                              (cdr list) 
                              (append urielements 
                                      '(nil) 
                                      '(nil))))
        ((null list) (uri-build urielements)) 
        ((is-path (list-to-sym list #\?)) 
         (query-opt (sym-to-list list #\?) 
                    (append urielements 
                            (list (list-to-sym list #\?)))))
        ((is-path (list-to-sym list #\#)) 
         (fragment-opt (sym-to-list list #\#)
                       (append urielements 
                               (list (sym-to-list list #\#)) 
                               '(nil))))
        (t (error "NOT VALID PATH 
          definition of PATH: 
          path :=  <identifier> [ / <identifier>]* '/'
          identifier := chars without {/ ? # @ :}"))))

;; Presenza facoltativa di
;; ['?' query]
(defun query-opt (list urielements)
  (cond ((is-query list) (uri-build (append urielements (list list))))
        ((is-query (list-to-sym list #\#)) 
         (fragment-opt (sym-to-list list #\#)
                       (append urielements 
                               (list (list-to-sym list #\#)))))
        (t (error "NOT VALID QUERY
          definition of QUERY:
          query := chars without {#}"))))

;; Presenza facoltativa di
;; ['#' fragment]
(defun fragment-opt (list urielements)
  (cond ((is-fragment list) (uri-build (append urielements (list list))))
        (t (error "NOT VALID FRAGMENT
          definition of FRAGMENT:
          fragment := chars"))))

;; Presenza facoltativa di
;; ['@' host]
(defun host-opt (list urielements)
  (if (is-host list) (uri-build (append urielements (list list)))
    (error "NOT VALID HOST
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}")))

;; Funzione che scompone l'URI in tutte le sue componenti
(defun uri-build (urielements)
  (make-uri :scheme (coerce-to-string (first urielements))
            :userinfo (coerce-to-string (second urielements))
            :host (coerce-to-string (third urielements))
            ;:port (if (and (or
            ;                (string=
            ;                 (coerce-to-string (first urielements))
            ;                 "http")
            ;                (string=
            ;                 (coerce-to-string (first urielements))
            ;                 "https")) 
            ;               (eq
            ;                (coerce-to-string (fourth urielements))
            ;                nil))
            ;          "80"
            ;        (coerce-to-string (fourth urielements)))
            :port (if
                      (eq (coerce-to-string (fourth urielements)) nil)
                      "80"
                    (coerce-to-string (fourth urielements)))
            :path (coerce-to-string (fifth urielements))
            :query (coerce-to-string (sixth urielements))
            :fragment (coerce-to-string (seventh urielements))))

;; Funzione di stampa di tutti gli elementi dell'URI
(defun uri-display (uri)
  (format t "Scheme:   ~A~%" (string-upcase (uri-scheme uri)))
  (format t "Userinfo: ~S~%" (uri-userinfo uri))
  (format t "Host:     ~S~%" (uri-host uri))
  (format t "Port:     ~A~%" (uri-port uri))
  (format t "Path:     ~S~%" (uri-path uri))
  (format t "Query:    ~S~%" (uri-query uri))
  (format t "Fragment: ~S~%" (uri-fragment uri))
  (values)) ; Utile a rimuovere NIL nell'output

;; FUNZIONI DI SUPPORTO
;; Rende gli output maggiormente comprensibili
(defun coerce-to-string (list)
  (if (null list) nil
    (coerce list 'string)))

;; Genera una lista di caratteri
(defun coerce-to-list (string)
  (coerce string 'list))

;; Questa funzione trasforma i numeri dal formato #\num in cifre
(defun to-number (n)
  (cond ((eq n #\0) 0)
        ((eq n #\1) 1)
        ((eq n #\2) 2)
        ((eq n #\3) 3)
        ((eq n #\4) 4)
        ((eq n #\5) 5)
        ((eq n #\6) 6)
        ((eq n #\7) 7)
        ((eq n #\8) 8)
        ((eq n #\9) 9)
        (t nil)))

;; Funzioni list-to-sym e sym-to-list che leggono la stringa
(defun list-to-sym (list sym)
  (if (eval (cons 'or (mapcar (lambda (x) (eq x sym)) list))) 
      (if (null list) nil
        (if (eq (car list) sym) nil
          (append (list (car list)) (list-to-sym (cdr list) sym)))) nil))

(defun sym-to-list (list sym)
  (if (null list) nil
    (if (eq (car list) sym) (cdr list)
      (sym-to-list (cdr list) sym))))

;; Genera una lista di sottostringhe utilizzando
;; come separatore il carattere '.'
(defun split-by-point (string)
  (loop for i = 0 then (1+ j)
        as j = (position '#\. string :start i)
        collect (subseq string i j)
        while j))

;;;; end of file -- uri-parse.lisp
