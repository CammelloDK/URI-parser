;;;; -*- Mode: Lisp -*-

;; Definizione struttura
;; :scheme :userinfo :host :port :path :query :fragment
(defstruct uri scheme userinfo host port path query fragment)

;; Funzione principale
;; uri-parse:  string -> list
(defun uri-parse (string)
    (if (null (coerce-to-list string)) (error "Empty is not a URI")
      (check-scheme (coerce-to-list string))))

;; Queste funzioni definiscono i vari elementi dell'URI
;; <query, fragment, host-identifier, identifier, digit>
(defun is-query (list)
  (if (null list) nil
    (eval (cons 'and (mapcar (lambda (char)
                               (if (eq char #\#)
                                   nil t))
                             list)))))

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
      (if (not (eq 
                (length 
                 (mapcar 'coerce-to-list (split-by-point 
                                          (coerce (coerce-to-string ip) 
                                                  'string)))) 
                4)) 
	  nil
          (if (and 
               (octet-length 
                (first 
                 (mapcar 'coerce-to-list (split-by-point 
                                          (coerce (coerce-to-string ip) 
                                                  'string)))))
                   (octet-length 
                    (second 
                     (mapcar 'coerce-to-list (split-by-point 
                                              (coerce (coerce-to-string ip) 
                                                      'string)))))
                   (octet-length 
                    (third 
                     (mapcar 'coerce-to-list (split-by-point 
                                              (coerce (coerce-to-string ip) 
                                                      'string)))))
                   (octet-length 
                    (fourth 
                     (mapcar 'coerce-to-list (split-by-point 
                                              (coerce (coerce-to-string ip) 
                                                      'string))))))
              t nil)))

;; Controlla che l'ottetto sia composto da 1 a 3 cifre
(defun octet-length (oct)
  (if (and (> (length oct) 0) (< (length oct) 4))
      (octet-is-number oct)
    nil))

;; Controlla che l'ottetto sia un numero
(defun octet-is-number (oct)
  (cond ((null oct) t)
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
    (if (is-scheme (list-to-sym list #\:)) 
        (cond ((equal (list-to-sym list #\:) '(#\m #\a #\i #\l #\t #\o)) 
               (mailto-scheme (sym-to-list list #\:) 
                              (list (list-to-sym list #\:))))
              ;((equal (list-to-sym list #\:) '(#\z #\o #\s))
               ;(zos-scheme s (list (list-to-sym list #\:))))
              ((equal (list-to-sym list #\:) '(#\n #\e #\w #\s)) 
               (news-scheme (sym-to-list list #\:) 
                            (list (list-to-sym list #\:))))
              ((or (equal (list-to-sym list #\:) '(#\t #\e #\l))
                   (equal (list-to-sym list #\:) '(#\f #\a #\x))) 
               (telfax-scheme (sym-to-list list #\:) 
                              (list (list-to-sym list #\:))))
              (t (if (and (eq (car (sym-to-list list #\:)) #\/)
                          (eq (car (cdr (sym-to-list list #\:))) #\/))
                     (first-uri-type (sym-to-list list #\:) 
                                     (list (list-to-sym list #\:))) 
                   (second-uri-type (sym-to-list list #\:) 
                                    (list (list-to-sym list #\:))))))
      (error "Not valid SCHEME: SCHEME IS OBLIGATORY
          definition of SCHEME:
          scheme := <identifier>
          identifier := chars without {/ ? # @ :}")))

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
(defun mailto-scheme (list urielements)
  ;(if (is-userinfo list) (uri-build (append urielements (list list)))
    (if (is-userinfo (list-to-sym list #\@)) 
        (host-opt (sym-to-list list #\@) 
                  (append urielements (list (list-to-sym list #\@))))
      (error "NOT VALID USERINFO: USERINFO IS OBLIGATORY
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}")))

;; NEWS
(defun news-scheme (list urielements)
  (if (is-host list) (uri-build (append urielements (list nil list)))
    (error "NOT VALID HOST: HOST IS OBLIGATORY
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}")))

;; TELFAX
(defun telfax-scheme (list urielements)
  (if (is-userinfo list) (uri-build (append urielements (list list)))
    (error "NOT VALID USERINFO: USERINFO IS OBLIGATORY
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}")))

;; ZOS
;(defun zos-scheme (list urielements)
;  (if () ()
;    (error "")))
  

;; Presenza facoltativa di
;; ['@' userinfo]
(defun userinfo-opt (list urielements) 
    (if 
        (is-userinfo (list-to-sym list #\@)) 
        (host-obb (sym-to-list list #\@) 
                  (append urielements (list (list-to-sym list #\@))))
      (if (and (null (list-to-sym list #\@)) (not (eq (car list) #\@)))
          (if (null (sym-to-list list #\@))
              (host-obb list (append urielements
                                     '(nil)))
            (host-obb (sym-to-list list #\@) (append urielements
                                '(nil) 
                                (list (list-to-sym list #\@)))))
                      
        (error "NOT VALID USERINFO
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}"))))

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
                               (list (list-to-sym list #\#)) 
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
            :port (if
                      (eq (coerce-to-string (fourth urielements)) nil)
                      80
                    (parse-integer 
                     (coerce-to-string (fourth urielements))))
            :path (coerce-to-string (fifth urielements))
            :query (coerce-to-string (sixth urielements))
            :fragment (coerce-to-string (seventh urielements))))

;; Funzione di stampa di tutti gli elementi dell'URI
(defun uri-display (uri &optional (stream *standard-output*))
  (format stream "Scheme:   ~A~%" (string-upcase (uri-scheme uri)))
  (format stream "Userinfo: ~A~%" (uri-userinfo uri))
  (format stream "Host:     ~S~%" (uri-host uri))
  (format stream "Port:     ~D~%" (uri-port uri))
  (format stream "Path:     ~S~%" (uri-path uri))
  (format stream "Query:    ~S~%" (uri-query uri))
  (format stream "Fragment: ~S~%~%" (uri-fragment uri))
  t)

;; FUNZIONI DI SUPPORTO
;; Rende gli output maggiormente comprensibili
;; list -> string
(defun coerce-to-string (list)
  (if (null list) nil
    (coerce list 'string)))

;; Genera una lista di caratteri
;; string -> list
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
  (if (position '#\. string) 
      (cons (subseq string 0 (position '#\. string)) 
            (split-by-point(subseq string (1+ (position '#\. string)))))
    nil))

;;;; end of file -- uri-parse.lisp
