;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "07" "q1.rkt")

(provide
 program-to-strings
 make-def
 make-varexp
 make-appexp)

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

;; TEMPLATE
;; def-fn : Def -> ??
;; (define (def-fn d)
;;    (...
;;       (def-name d)
;;       (def-args d)
;;       (def-body d)))

(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; TEMPLATE
;; varexp-fn : Varexp -> ??
;; (define (varexp-fn v)
;;   (...
;;       (varexp-name v)))

;; appexp-fn : Appexp -> ??
;; (define (appexp-fn a)
;; (...
;;    (appexp-fn a)
;;    (apppexp-args a)))

;; A ListOfExpression (LOE) is:
;; -- empty
;; --(cons Expression LOE)

;; TEMPLATE:
;; exp-fn : Exp -> ?
;; (define (exp-fn f)
;;     (cond
;;       [(varexp? f) (... (varexp-name f))]
;;       [(appexp? f) (...
;;                     (exp-fn (appexp-fn f))
;;                     (exp-fn (appexp-args f)))]))

;; A ListOfString (LOS) is:
;; -- empty
;; -- (cons String ListOfString)

;; TEMPLATE
;; los-fn: LOS->??
;;  (define (los-fn LOS)
;;     (cond
;;       [(empty? LOS) ...]
;;       [else (...
;;           (string-fn (first LOS))
;;           (los-fn (rest LOS)))]))

;; A ListOfSymbol (LOSM) is:
;; -- empty
;; -- (cons Symbol ListOfSymbol)

;; TEMPLATE
;; losm-fn: LOSM->??
;;  (define (losm-fn LOSM)
;;     (cond
;;       [(empty? LOSM) ...]
;;       [else (...
;;           (symbol-fn (first LOSM))
;;           (losm-fn (rest LOSM)))]))

;; A ListOfDefinition (LOD) is:
;; -- empty
;; -- (cons Definition LOD)

;; TEMPLATE
;; lod-fn: LOD -> ??
;; (define (lod-fn lod)
;;   (cond
;;     [(empty? lod) ...]
;;     [else (...
;;           (def-fn (first lod))
;;           (lod-fn (rest lod)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define COMMA ",")
(define OPENING-PARANTHESIS "(")
(define CLOSING-PARANTHESIS ")")
(define COLON ":")
(define DEF "def")
(define NEW-LINE "\n")
(define SPACE " ")
(define NO-SPACE "")
(define ZERO 0)
(define ONE 1)
(define DEFAULT-INDENT-BODY 4)
(define SPACE-CHAR #\space)
(define CLOSING-PARANTHESIS-CHAR #\))

(define SAMPLE-INPUT
  (make-appexp
   'f1
   (list (make-appexp
          'f2 (list (make-varexp 'a)
                    (make-varexp 'b)
                    (make-varexp 'c)
                    ))
         (make-varexp 'only)
         (make-appexp
          'f1 (list (make-appexp 'f2
                                 (list
                                  (make-varexp 'm)
                                  (make-varexp 'n)))
                    (make-varexp 'last_z))))))

(define sample-program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa '()))))

(define SAMPLE-PROGRAM1
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'ffdsbgdghg1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'zabcdeflonglonglong)
                               (make-varexp 'yml)))
                    (make-varexp 'dsdsaddasdad)
                    (make-varexp 'dsdsaddasdad)
                    (make-varexp 'dsdsaddasdad)
                    (make-varexp 'dsdsaddasdad)
                    
                    (make-appexp
                     'f1 '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN    : GarterSnake program and a width
;; RETURNS  : a representation of the program as a sequence of lines,
;;            following the formatting rules described below.
;; EAMPLES  : see test cases.
;; STRATEGY : Use HOF foldr on ListOfDefinition - def_list.

(define (program-to-strings def_list limit)
  ;; (Definition LOS -> LOS) LOS INT -> ListOfString
  (foldr
   ;; (Definition LOS) -> LOS
   ;; RETURNS -> definition converted to LOS and appending it base LOS.
   ;; it to string.
   (lambda(x base)
     (append
      (append
       (get-defination-name-and-args
        (symbol->string (def-name x)) (def-args x) limit)
       (getBody (def-body x) limit))
      base))
   
   empty def_list))



;; TESTS
(begin-for-test
  (check-equal?  (program-to-strings sample-program 25)
                 (list
                  "def a-very-long-function-name (x) :"
                  "    f1(x)"
                  "def f2 (x,"
                  "        a-very-long-variable-name,"
                  "        y) :"
                  "    f1(y)"
                  "def f3 (x,z,t,u) :"
                  "    f1aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                  "    ()")
                 "incorrect conversion of program to list of string"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Slist->StringList : LOSM INT -> LOS
;; GIVEN    : a list of symbol and an offset stating the indentation for that
;; symbol.
;; RETURNS  : a list of strings with each string indented as given.
;; EXAMPLE  : (Slist->StringList (list 'x 'a-very-long-variable-name 'y) 10)
;;          (list "(x," "          a-very-long-variable-name,"
;;                      "          y) :"
;; STRATEGY : use HOF foldr on LOSM - slist.

(define (Slist->StringList slst offset)
  ;; (Symbol LOS -> LOS) LOS INT -> ListofStrings.
  (foldr
   ;; (Symbol LOS) -> LOS
   ;; RETURNS -> List of string appending the converted symbol to string.
   (lambda (s base)
     (cond
       [(equal? base empty) (cons (string-append
                                   (if (= (length slst) ONE)
                                       OPENING-PARANTHESIS
                                       (getArrayOfSpaces offset))
                                   (symbol->string s)
                                   CLOSING-PARANTHESIS SPACE COLON) base)]
       
       [(= (length base) (- (length slst) ONE))
        (cons (string-append OPENING-PARANTHESIS
                             (symbol->string s) COMMA) base)]
       [else
        (cons (string-append
               (getArrayOfSpaces offset)
               (symbol->string s)
               COMMA) base)]))
   
   empty slst))

;; TESTS
(begin-for-test
  (check-equal? (Slist->StringList (list 'x 'a-very-long-variable-name 'y) 10)
                (list "(x," "          a-very-long-variable-name,"
                      "          y) :")
                "Slist->StringList converted symbols incorrectly"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Slist->String : LOSM -> STRING
;; GIVEN    : a list of symbol
;; WHERE    : delimiters COMMA and SPACE will be applied to all symbols
;; but last.
;; RETURNS  : a string made out of symbols with appropriate delimiters.
;; EXAMPLE  : see test cases below.
;; STRATEGY : use HOF foldr on LOSM - slist.

(define (Slist->String slst)
  (string-append
   (apply-paranthesis
    ;; (Symbol String -> String) String ListOfSymbol -> String
    (foldr
     ;; (Symbol String) -> String
     ;; RETURNS -> a string appending the symbol to it by converting
     ;; it to string.
     (lambda (s base)
       (if (equal? base "")
           (symbol->string s)
           (string-append (symbol->string s)
                          COMMA 
                          base))) "" slst)) SPACE COLON))

;; TESTS
(begin-for-test
  (check-equal? (Slist->String (list 'x 'a-very-long-variable-name 'y))
                "(x,a-very-long-variable-name,y) :"
                "Slist->String converted symbols incorrectly"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; getArrayOfSpaces : INT -> STRING
;; GIVEN    : a number stating the number of spaces
;; RETURNS  : a string made out of spaces as given.
;; EXAMPLE  : see test cases below.
;; STRATEGY : combine simpler functions.

(define (getArrayOfSpaces num)
  (build-string num (lambda (i) SPACE-CHAR)))

;; TESTS
(begin-for-test
  (check-equal? (getArrayOfSpaces 10) "          "
                "Incorrect generation of spaces"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apply-paranthesis : String -> STRING
;; GIVEN    : a String to wrap paranthesis around.
;; RETURNS  : a string wraped with paranthesis around.
;; EXAMPLE  : see test cases below.
;; STRATEGY : combine simpler functions.
(define (apply-paranthesis data)
  (string-append OPENING-PARANTHESIS data CLOSING-PARANTHESIS))

;; TESTS
(begin-for-test
  (check-equal? (apply-paranthesis "test") "(test)"
                "Incorrect generation of paranthesis"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get-defination-name-and-args : String LOSM INT -> LOS
;; GIVEN    : a name of the defination, its symbol list and limit to fit def.

;; RETURNS  : list of string fitting either in one or multiple line depending
;; upon the limit.
;; EXAMPLE  : see test cases below.
;; STRATEGY : combine simpler functions.

(define (get-defination-name-and-args name args limit)
  
  (if (> (string-length (first (append-def-name name
                                                (list (Slist->String args)))))
         limit)
      (append-def-name name (Slist->StringList args
                                               (+ (string-length
                                                   (string-append DEF
                                                                  SPACE
                                                                  name
                                                                  SPACE))
                                                  ONE)))
      (append-def-name name (list (Slist->String args)))))

;; TESTS
(begin-for-test

  (check-equal? (get-defination-name-and-args
       "test" (list 'x 'a-very-long-variable-name 'y) 10)
                (list "def test (x," "          a-very-long-variable-name,"
                      "          y) :")
                "incorrect definitions are produced. check for indentation.")

  (check-equal? (get-defination-name-and-args

                 "test" (list 'x 'a-very-long-variable-name 'y) 50)
                (list "def test (x,a-very-long-variable-name,y) :")
                "incorrect definitions are produced. check for indentation."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; append-def-name : String LOS -> LOS
;; GIVEN    : a name of the definition, its symbol list as string list.
;; RETURNS  : list of string appending definition meta to first string in list.
;; EXAMPLE  : see test cases below.
;; STRATEGY : combine simpler functions

(define (append-def-name name lst)
  (if (empty? lst)
      (cons (string-append DEF SPACE name SPACE
                           OPENING-PARANTHESIS CLOSING-PARANTHESIS
                           SPACE COLON) empty)
      (cons (string-append DEF SPACE name SPACE (first lst)) (rest lst))))

;; TESTS
(begin-for-test
  (check-equal? (append-def-name "test"
                                 (list "(x,"
                                       "          a-very-long-variable-name,"
                                       "          y) :"))
                (list "def test (x,"
                      "          a-very-long-variable-name,"
                      "          y) :")
                "Incorrectly appended header to argument list")
  
  (check-equal? (append-def-name "test" empty)
                (list "def test () :")
                "Incorrectly appended header to argument list"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-exp-body-for-case1 : Exp -> String
;; GIVEN    : an Exp
;; RETURNS  : the given expression as a single string.
;; EXAMPLE  : see test cases below.
;; STRATEGY : Divide on cases for Exp on x
(define (get-exp-body-for-case1 x)
  (cond
    [(varexp? x) (symbol->string (varexp-name x))]
    [(appexp? x)
     (string-append
      (symbol->string (appexp-fn x))
      (apply-paranthesis
       (get-exp-arg-for-case1 (appexp-args x)))
      )]))

;; TESTS
(begin-for-test
  (check-equal? (get-exp-body-for-case1 (make-appexp
                                         'f1 (list (make-varexp 'x))))
                "f1(x)"
                "get-exp-body-for-case1 returned other than expected input")
  (check-equal? (get-exp-body-for-case1 (make-varexp 'x))
                "x"
                "get-exp-body-for-case1 returned other than expected input"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-exp-arg-for-case1 : ListOfExp -> String
;; GIVEN    : an List of Exp
;; RETURNS  : a string representing the given list of Expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY : Use HOF foldr on ListOfExp - lst.
;; Halting Measure : Lenght of list. The function will halt when all
;; the expression and its child expressions are processed.
(define (get-exp-arg-for-case1 lst)
  ;; (Exp String -> String) String ListOfExp -> String
  (foldr
   ;; (Exp String) -> String
   ;; RETURNS -> a string by converting expression into string
   (lambda(x base)
     (string-append
      (if (varexp? x)
          (symbol->string (varexp-name x))
          (get-exp-body-for-case1 x))
      
      (if (equal? "" base)
          NO-SPACE
          (string-append COMMA SPACE))
      
      base))    
   NO-SPACE lst))

;; TESTS
(begin-for-test
  (check-equal? (get-exp-arg-for-case1 (list (make-appexp
                                              'f2 (list (make-varexp 'z)
                                                        (make-varexp 'y)))))
                "f2(z, y)"
                "incorrect results for given list of expression"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; case-handler : Exp List PosInt PosInt PosInt -> ListOfString
;; GIVEN    : an Exp,
;;            flist represents a list.
;;            ispace a positive integer 
;;            len represents the length of args
;; WHERE    : ispace denotes the number of spaces to be appended
;;            before expression
;;            len keeps the track of arguments of an expression.
;; RETURNS  : a list of string representing the expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY : Divide on cases based on length.

(define (case-handler exp flist ispace limit len)
  (cond   
    [(< (+ (string-length (get-exp-body-for-case1 exp)) ispace) limit)
     (append flist (list
                    (string-append
                     (getArrayOfSpaces  ispace)
                     (get-exp-body-for-case1 exp))))]
    
    [(and (not (empty?  (appexp-args exp)))
          (< (+ (string-length
                 (get-exp-body-for-case1
                  (make-appexp
                   (appexp-fn exp) (list (first (appexp-args exp))))))
                ispace) limit))     
     (append flist (get-exp-body-for-case2 exp ispace limit len))]
    [else
     (append flist (get-exp-body-for-case3 exp ispace limit len))]))

;; TESTS
(begin-for-test
  (check-equal? (case-handler (make-appexp
                               'f2 (list (make-varexp 'z)
                                         (make-varexp 'y)))
                              '() 4 20 0)
                (list "    f2(z, y)")
                "case-handler returned incorrect list of strings for
                 given expression")
  (check-equal? (case-handler (make-appexp
                               'f1
                               (list (make-appexp
                                      'f2 (list (make-varexp 'z)
                                                (make-varexp 'y)))
                                     (make-varexp 'z)
                                     (make-appexp
                                      'f1 (list (make-appexp
                                                 'f2 (list (make-varexp 'z)
                                                           (make-varexp 'y)))
                                                (make-varexp 'z)))))
                              '() 4 20 0)
                (list "    f1(f2(z, y),"
                      "       z,"
                      "       f1(f2(z, y),"
                      "          z))")
                "case-handler returned incorrect list of strings for
                 given expression"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-exp-body-for-case2 : Exp String List String Int -> ListOfString
;; GIVEN    : an Exp,
;;            ispace a positive integer.
;;            len represents the length of args
;; WHERE    : ispace denotes the number of spaces to be appended
;;            before expression
;; RETURNS  : a list of string representing the expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY : Combine simpler functions
(define (get-exp-body-for-case2 exp ispace limit len)
  (cons 
   (string-append
    (getArrayOfSpaces ispace)
    (symbol->string (appexp-fn exp))
    OPENING-PARANTHESIS
    (get-exp-body-for-case1  (first (appexp-args exp)))
    COMMA)
   (get-exp-args-for-case2 (rest (appexp-args exp))
                           (+ (name-length exp) ispace ONE) limit
                           (length (rest (appexp-args exp))))))


;; TESTS
(begin-for-test
  (check-equal? (get-exp-body-for-case2 (make-appexp
                                         'f2 (list (make-varexp 'z)
                                                   (make-varexp 'y))) 4 20 0)
                (list "    f2(z," "       y)")
                "get-exp-body-for-case2 returned incorrect list of strings"))



;; get-exp-args-for-case2 : ListOfExp String List String Int -> ListOfString
;; GIVEN    : a list of Expression,
;;            ispace a positive integer denoting the number of spaces.
;;            len represents the length of args
;; RETURNS  : a list of string representing the expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY : Divide on cases for arg length.
;; Halting Measure : Lenght of list. The function will halt when all
;; the expression and its child expressions are processed.
(define (get-exp-args-for-case2 args ispace limit len)
  
  (cond
    [(empty? args) empty]
    [else
     (cond
       [(= (length (rest args)) 0)
        (append
         (append-delimiter-at-end
          (get-remaining-exp-body (first args)
                            ispace limit len) CLOSING-PARANTHESIS)
         (get-exp-args-for-case2 (rest args) ispace limit len)) ]
       [else
        (append
         (append-delimiter-at-end
          (get-remaining-exp-body (first args)
                            ispace limit len) COMMA)
         (get-exp-args-for-case2 (rest args) ispace limit len))])]))

;; TESTS
(begin-for-test
  (check-equal? (get-exp-args-for-case2  (list (make-varexp 'z)
                                               (make-varexp 'y)) 4 20 0)
                
                (list "    z," "    y)")
                "get-exp-args-for-case2 returned incorrect list of strings"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-exp-body-for-case3 : Exp String List String Int -> ListOfString
;; GIVEN    : an Exp,
;;            ispace a positive integer
;;            len represents the length of args
;; WHERE    : ispace denotes the number of spaces to be appended
;;            before expression
;; RETURNS  : a list of string representing the expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY : Combine simpler functions

(define (get-exp-body-for-case3 exp ispace limit len)
  (cons (string-append
         (getArrayOfSpaces ispace)
         (symbol->string (appexp-fn exp)))
        (if (<= (length (appexp-args exp)) ONE)
            (handle-single-expression-for-case3 exp ispace limit len)
            (get-exp-args-for-case3 (appexp-args exp)
                                    (+ 1 ispace) limit
                                    (length (appexp-args exp))))))

;; TESTS
(begin-for-test
  (check-equal? (get-exp-body-for-case3 (make-appexp
                                         'f2 (list (make-varexp 'z)
                                                   (make-varexp 'y))) 4 2 0)
                (list "    f2" "     (z," "      y)")
                "get-exp-body-for-case3 returned incorrect list of strings"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle-single-expression-for-case3 :
;;            Exp String List String Int -> ListOfString
;; GIVEN    : an Exp,
;;            ispace a positive integer denoting the number of spaces.
;;            len represents the length of args
;; RETURNS  : a list of string representing the expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY :  Divide on cases for arg length.
(define (handle-single-expression-for-case3 exp ispace limit len)
  (cond
    [(empty? (appexp-args exp)) (list  (string-append
                                        (getArrayOfSpaces  ispace)
                                        OPENING-PARANTHESIS
                                        CLOSING-PARANTHESIS))]
    [( = (length (get-remaining-exp-body
                  (first (appexp-args exp)) ispace limit len)) ONE)
     (list
      (string-append
       (first
        (get-remaining-exp-body
         (append-opening-paranthesis
          (first (appexp-args exp))) ispace limit len))
       CLOSING-PARANTHESIS))]
    [else (append-delimiter-at-end
           (get-remaining-exp-body
            (append-opening-paranthesis
             (first (appexp-args exp))) ispace limit len)
           CLOSING-PARANTHESIS)]))

;; TESTS
(begin-for-test
  (check-equal? (handle-single-expression-for-case3
                 (make-appexp 'f2 '()) 4 2 0)
                (list "    ()")
                "handle-single-expression-for-case3
                  returned incorrect list of strings")
  (check-equal? (handle-single-expression-for-case3
                 (make-appexp
                  'f2 (list (make-varexp 'z)
                            (make-varexp 'y))) 4 2 0)
                (list "    (z)")
                "handle-single-expression-for-case3
                  returned incorrect list of strings")
  (check-equal? (handle-single-expression-for-case3
                 (make-appexp
                  'f1 (list (make-appexp
                             'f2 (list (make-varexp 'z)
                                       (make-varexp 'y)))
                            (make-varexp 'z))) 4 3 0)
                (list "    (f2" "     (z," "      y))")
                "handle-single-expression-for-case3
                  returned incorrect list of strings"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get-exp-args-for-case3 : ListOfExp String List String Int -> ListOfString
;; GIVEN    : an List of Expression,
;;            ispace a positive integer denoting the number of spaces.
;;            len represents the length of args
;; RETURNS  : a list of string representing the expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY :  Divide on cases for arg length.
;; Halting Measure : Lenght of list. The function will halt when all
;; the expression and its child expressions are processed.
(define (get-exp-args-for-case3 args ispace limit len)
  (cond
    [(empty? args) empty]
    [else
     (cond
       [(= (length (rest args)) 0)
        (append
         (append-delimiter-at-end
          (get-remaining-exp-body
           (first args)  (+ ispace 1)  limit len)
          CLOSING-PARANTHESIS)
         (get-exp-args-for-case3 (rest args) ispace limit len)) ]
       [(= (length (rest args)) (- len 1))
        (append
         (append-delimiter-at-end
          (get-remaining-exp-body
           (append-opening-paranthesis (first args)) ispace  limit len)
          COMMA)
         (get-exp-args-for-case3 (rest args) ispace limit len))]
       [else
        (append
         (append-delimiter-at-end
          (get-remaining-exp-body (first args)  (+ ispace 1)  limit len)
          COMMA) (get-exp-args-for-case3 (rest args) ispace limit len))])]))


;; TESTS
(begin-for-test
  (check-equal? (get-exp-args-for-case3
                 (list (make-varexp 'z) (make-varexp 'y)) 4 10 0)
                (list "     z," "     y)")
                "get-exp-args-for-case3 returned incorrect list of strings"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-remaining-exp-body : Exp String List String Int -> ListOfString
;; GIVEN    : an List of Expression,
;;            ispace a positive integer denoting the number of spaces.
;;            len represents the length of args
;; RETURNS  : a list of string representing the expression.
;; EXAMPLE  : see test cases below.
;; STRATEGY :  combine simpler functions.

(define (get-remaining-exp-body exp ispace limit len)
  (if (varexp? exp)
      (list (string-append (getArrayOfSpaces ispace)
                           (symbol->string (varexp-name exp))))
      (case-handler exp '() ispace limit len)))                

;; TESTS
(begin-for-test
  (check-equal? (get-remaining-exp-body (make-varexp 'z) 4 10 0)
                (list "    z")
                "get-remaining-exp-body returned incorrect list of strings"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; append-delimiter-at-end : LOS STRING -> STRING
;; GIVEN    : an listOfString and a delimiter which is a string.
;; RETURNS  : the same list as given but delimiter appended at last string of
;; list.
;; EAMPLES  : see test cases below.
;; STRATEGY : Use HOF foldr on LOS - lst.
(define (append-delimiter-at-end lst delimeter)
  ;; (String LOS -> LOS) LOS String -> ListOfString
  (foldr
   ;; (String LOS) -> LOS
   ;; RETURNS -> Same list of strings as given adding given delimeter at the
   ;; end.
   (lambda (x base)
     (if (equal? base empty)
         (cons (string-append x delimeter) base)
         (cons x base)))
   empty lst))

;; TESTS
(begin-for-test
  (check-equal? (append-delimiter-at-end
                 (list "testing" "paranthesis") CLOSING-PARANTHESIS)
                (list "testing" "paranthesis)")
                "Appending strings incorrectly"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; append-opening-paranthesis : Exp -> Exp
;; GIVEN    : an Expression.
;; RETURNS  : the same expression with opening braces appended at the start of
;; Expression name.
;; EXAMPLES  : see test cases below.
;; STRATEGY : combine simpler funtions.
(define (append-opening-paranthesis exp)
  (if (varexp? exp)
      (make-varexp
       (string->symbol
        (string-append OPENING-PARANTHESIS (symbol->string (varexp-name exp)))))
      (make-appexp
       (string->symbol
        (string-append OPENING-PARANTHESIS (symbol->string (appexp-fn exp))))
       (appexp-args exp))
      ))

;; TESTS
(begin-for-test
  (check-equal? (append-opening-paranthesis (make-appexp 'f2 '()))
                (make-appexp '|(f2| '())
                "incorrect length of the program")
  (check-equal? (append-opening-paranthesis (make-varexp 'f2))
                (make-varexp '|(f2|)
                "incorrect length of the program"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; name-length : Exp -> Int
;; GIVEN    : an Expression.
;; RETURNS  : the length of the name of the Expression.
;; EAMPLES  : see test cases below.
;; STRATEGY : combine simpler funtions.
(define (name-length exp)
  (if (appexp? exp)
      (string-length (symbol->string (appexp-fn exp)))
      (string-length (symbol->string (varexp-name exp)))))

;; TESTS
(begin-for-test
  (check-equal? (name-length (make-appexp 'f2 '()))
                2
                "incorrect length of the name of exp")
  (check-equal? (name-length (make-varexp 'f2 ))
                2
                "incorrect length of the name of exp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; getBody : Exp PosInt -> LOS
;; GIVEN    : an Expression and a limit to fit it.
;; RETURNS  : list of string consisting of the body of Definition.
;; EXAMPLES  : see test cases below --tbd
;; STRATEGY : combine simpler funtions.
(define (getBody exp limit)
  (case-handler exp empty DEFAULT-INDENT-BODY limit ZERO))

;; TESTS
(begin-for-test
  (check-equal? (getBody  (make-appexp 'f2 '()) 10)
                (list "    f2()")
                "getBody returned incorrect list of exp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;END
