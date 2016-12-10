;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
	 make-enrollment
         enrollment-student
         enrollment-class
         make-roster
         behavior-correct?
         enrollments-to-rosters
         enrollments-to-rosters-bad-3
         enrollments-to-rosters-bad-2
         enrollments-to-rosters-bad-1
         roster-classname
         roster-students)


;*******************************************************************



(define-struct enrollment (student class))
;; An EnrollmentAssertion is a (make-enrollment Student Class).
;; (make-enrollment s c) represents the assertion that student s is
;; enrolled in class c.
;; Student and Class can be any data type that can be compared to
;; other Student and Class objects using equal?.

;; template:
;; enrollment-fn : Enrollment -> ??
;; (define (enrollment-fn e)
;; (... (enrollment-student e)
;;      (enrollment-class e))


;; A SetOfEnrollmentAssertion (SEA) is either
;; -- empty
;; -- (cons Enrollment SEA)

;; TEMPLATE
;; sea-fn : SEA -> ??
;; (define (sea-fn lst)
;; (cond
;;     [(empty? lst) ...]
;;     [else (...
;;             (enrollment-fn (first lst))
;;             (sea-fn (rest lst)))]))


(define-struct roster (classname students))
;; A ClassRosterAssertion is a (make-roster Class SetOfStudent).
;; (make-roster c ss) represents the assertion that the students in class
;; c are exactly the students in set ss.
;; Classname is the name of the class, students are enrolled in.
;; Class can be any data type.
;; Students is the list of students enrolled for the given class.
;; Students can be of any data type.

;; template:
;; roster-fn : Roster -> ??
;; (define (roster-fn r)
;; (... (roster-classname r)
;;      (roster-students r))


;; DATA DEFINITIONS FOR TESTING
(define-struct student (firstname lastname))
;; A Student is a (make-student String String)
;; firstname and lastname are the first and last name of a student.

;; template:
;; student-fn : Student -> ??
;; (define (student-fn s)
;; (... (student-firstname s)
;;      (student-lastname s))

(define-struct class (name id))
;; A Class is a (make-class String PosInt)
;; name : It is the name of the class
;; id : It is the unique id to distinguish the class

;; template:
;; class-fn : Class -> ??
;; (define (class-fn c)
;; (... (class-name c)
;;      (class-id c))


;; A SetOfStudent (SOS) is either
;; -- empty
;; -- (cons Student LOS)

;; TEMPLATE
;; sos-fn : SOS -> ??
;; (define (sos-fn lst)
;; (cond
;;     [(empty? lst) ...]
;;     [else (...
;;             (student-fn (first lst))
;;             (sos-fn (rest lst)))]))


;; A SetOfClassRosterAssertion (SCA) is either
;; -- empty
;; -- (cons Roster SCA)

;; TEMPLATE
;; sca-fn : SCA -> ??
;; (define (sca-fn lst)
;; (cond
;;     [(empty? lst) ...]
;;     [else (...
;;             (roster-fn (first lst))
;;             (sca-fn (rest lst)))]))


;; A ProposedSolution is a function with contract
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; that is, it is a function that takes a SetOfEnrollmentAssertion and
;; produces a SetOfClassRosterAssertion


;; DATA DEFINITION ENDS
;*******************************************************************

;; enrollments-to-rosters :
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN    : a set of enrollments
;; RETURNS  : a correct set of class rosters for the given enrollments 
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF map on SetOfEnrollmentAssertion
(define (enrollments-to-rosters ea) 
  (filter
   ; ClassRosterAssertion -> Boolean
   ; RETURNS : true iff the length of student list is greater than 0.
   (lambda (r)
    (not (empty? (first (roster-students r)))))
     (map
      ; Class -> Roster
      ; RETURN : a roster for that class.
      (lambda (c)
       (make-roster c (filter-duplicates equal?
                      (provide-students-for-class c ea))))
        (provide-class-set ea))))


;;test
(begin-for-test
  (check-equal? (enrollments-to-rosters
                 (list (make-enrollment "John" "PDP")
                       (make-enrollment "John" "PDP")
                       (make-enrollment '() "BMS")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                
                (list
                 (make-roster "PDP" (list "John" "Feng" "Amy"))
                 (make-roster "Networks" (list "Kathryn" "Amy")))
                "enrollments-to-rosters returned unexpected output")

  (check-equal? (enrollments-to-rosters
                 (list (make-enrollment (make-student "John" "D")
                                        (make-class "PDP" 1))
                       (make-enrollment (make-student "Amy" "C")
                                        (make-class "PDP" 1))
                       (make-enrollment (make-student "Suraj" "E")
                                        (make-class "Networks" 2))
                       (make-enrollment (make-student "John" "D")
                                        (make-class "Networks" 2))))
                 (list
                       (make-roster (make-class "PDP" 1)
                                  (list (make-student "John" "D")
                                        (make-student "Amy" "C")))
                       (make-roster (make-class "Networks" 2)
                                  (list (make-student "Suraj" "E")
                                        (make-student "John" "D"))))
                "enrollments-to-rosters returned unexpected output"))

;*******************************************************************


;; provide-students-for-class : Class SetOfEnrollmentAssertion -> ListOfStudents
;; GIVEN    : a class and a set of enrollments
;; RETURNS  : a list of students belonging to that class.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF foldr on SetOfEnrollmentAssertion
(define (provide-students-for-class c ea)
  (foldr
        ;(Enrollment LOS -> LOS)
        ;RETURNS : ListOfStudent whose class matches the given class 'c'.
        (lambda (e x)
          (if (equal? (enrollment-class e) c)
                       (cons (enrollment-student e) x) x))
        empty ea))

;;test
(begin-for-test
  (check-equal? (provide-students-for-class "PDP"
                 (list
                       (make-enrollment "John" "PDP")
                       (make-enrollment '() "BMS")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                (list "John" "Feng" "Amy")
                "provide-students-for-class returned incorrect classes"))

;*******************************************************************


;; provide-class-set : SetOfEnrollmentAssertion -> ListOfClass
;; GIVEN    : a set of enrollments
;; RETURNS  : a unique list of class.
;; EXAMPLES : See test cases
;; STRATEGY : combine simpler functions.
(define (provide-class-set ea)
  (filter-duplicates equal? (provide-class-list ea)))


;;test
(begin-for-test
  (check-equal? (provide-class-set
                 (list (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                  (list "PDP" "Networks")
                   "provide-class-set returned incorrect set"))

;*******************************************************************


;; filter-duplicates : Filter-Fn ListOfX -> ListOfX
;; GIVEN    : a list of X where X can be of type Any
;; and a function Filter-Fn that checks the equality of the X elements.
;; RETURNS  : a list of X without duplicates
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF foldr on ListOfX - lst
(define (filter-duplicates filter-fn lst)
  ;(X LOX -> LOX) LOX ListOfX -> ListOfX
  (foldr    
     ;(X LOX -> LOX)
     ;RETURNS : ListOfX without duplicates
   (lambda (x y)
           (cons x
                 (filter
                  ;; X -> Boolean
                  ;; RETURNS : true iff any x from
                  ;; list y is similar to x
                    (lambda (z)
                      (not (filter-fn x z))) y))) empty lst))

;;test
(begin-for-test
  (check-equal? (filter-duplicates
                 equal? (list "PDP" "Networks" "PDP" "PDP" "Networks"))
                (list "PDP" "Networks")
                "filter-duplicates returned duplicates or unexpected value"))

;*******************************************************************


;; provide-class-list : SetOfEnrollmentAssertion -> ListOfClass
;; GIVEN    : a set of enrollments
;; RETURNS  : a list of class in those enrollments with duplicates if present
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF map on SetOfEnrollmentAssertion
(define (provide-class-list ea)
  (map
      ;; Enrollment -> Class
      ;; RETURNS : The class in given enrollment object.
      (lambda (e)
        (enrollment-class e)) ea))

;;test
(begin-for-test
  (check-equal? (provide-class-list
                 (list (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                  
                (list "PDP" "Networks" "PDP" "PDP" "Networks")
                "provide-class-list returned improper list"))

;*******************************************************************


;; behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
;; GIVEN    : a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
;; RETURNS  : true iff the output of soln-fn on se is an example of correct
;; behavior by a ProposedSolution.
;; STRATEGY : combine simpler functions.
;; EXAMPLE  : See example above
(define (behavior-correct? ProposedSolution ea)
  (contain-same-classes-and-student
   (enrollments-to-rosters ea) (ProposedSolution ea)))
      

;; test
(begin-for-test
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-1
                   (list (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks"))) false
                        "behavior-correct? produced incorrect result
                         enrollments-to-rosters-bad-1 test 1")
  
   (check-equal? (behavior-correct? enrollments-to-rosters-bad-1
                   (list (make-enrollment "Amy" "Networks"))) false
                        "behavior-correct? produced incorrect result
                         enrollments-to-rosters-bad-1 test 2")


   (check-equal? (behavior-correct? enrollments-to-rosters-bad-2
                 (list 
                       (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment  empty "BMS")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks"))) false
                       "behavior-correct? produced incorrect result
                         enrollments-to-rosters-bad-2 test 1")

  (check-equal? (behavior-correct? enrollments-to-rosters-bad-2
                   (list (make-enrollment "Amy" "Networks"))) false
                        "behavior-correct? produced incorrect result
                         enrollments-to-rosters-bad-2 test 2")

  (check-equal? (behavior-correct? enrollments-to-rosters-bad-3
                   (list (make-enrollment "Amy" "Networks"))) false
                        "behavior-correct? produced incorrect result
                         for enrollments-to-rosters-bad-3 test 1"))


;*******************************************************************


;; provide-class-list :
;; SetOfEnrollmentAssertion SetOfEnrollmentAssertion -> boolean
;; GIVEN    : a correct and incorrect list of enrollments.
;; RETURNS  : a boolean if the two list has same classes and those classes
;; have same students.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF andmap on SetOfEnrollmentAssertion
(define (contain-same-classes-and-student clist ilist)
  (if (= (length clist) (length ilist)) 
      (andmap
          ;; Roster -> Boolean
          ;; RETURNS : true iff the correct behaviour list contains class and
          ;; corresponding behaviour.
          (lambda (r)
            (and (has-class? (roster-classname r) clist)
                  (contain-same-students?
                   (getStudentsForClass (roster-classname r) clist)
                   (getStudentsForClass (roster-classname r) ilist)))) ilist)
  false))

(begin-for-test
  (check-equal? (contain-same-classes-and-student
                 (list
                  (make-roster "PDP" (list "John" "Feng" "Amy"))
                  (make-roster "Networks" (list "Kathryn" "Amy")))
                 (list              
                  (make-roster "Networks" (list "Kathryn" "Amy"))
                  (make-roster "PDP" (list "Feng" "Amy"  "John" )))) true
                  "contain-same-classes-and-student returned incorrect output")

  (check-equal? (contain-same-classes-and-student
                (list
                 (make-roster "PDP" (list "John" "Feng" "Amy"))
                 (make-roster "Networks" (list "Kathryn" "Amy")))
                (list
                 (make-roster "Netorks" (list "Kathryn" "Amy"))
                 (make-roster "PDP" (list "Feng" "Amy"  "John" )))) false
                 "contain-same-classes-and-student returned incorrect output"))


;*******************************************************************

;; has-class? : Class ListofEnrollmentRoster -> boolean
;; GIVEN    : a Class and list of enrollment roster.
;; RETURNS  : true iff the given list contains the class given.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF ormap on ListofEnrollmentRoster
(define (has-class? class lst)
  (ormap
         (lambda (r)
           (equal? (roster-classname r) class)) lst))

(begin-for-test
  (check-equal? (has-class? "PDP"  (list
                
                 (make-roster "Netorks" (list "Kathryn" "Amy"))
                  (make-roster "PDP" (list "Feng" "Amy"  "John" )))) true
                   "has-class? returned incorrect output")
  
 (check-equal? (has-class? "PP"  (list
                
                 (make-roster "Netorks" (list "Kathryn" "Amy"))
                  (make-roster "PDP" (list "Feng" "Amy"  "John" )))) false
                    "has-class? returned incorrect output"))

;*******************************************************************

;; contain-same-students? : Class ListofEnrollmentRoster -> boolean
;; GIVEN    : a Class and list of enrollment roster.
;; RETURNS  : true iff the given list contains the class given.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF ormap on ListofEnrollmentRoster

(define (contain-same-students? clist ilist)
  (if (= (length clist) (length ilist)) 
      (andmap
          (lambda (s)
            (has-student? s clist)) ilist)
  false))

(begin-for-test
  (check-equal? 
   (contain-same-students?
    (list "Kathryn" "Amy") (list "Amy" "Kathryn" )) true
     "contain-same-students? returned incorrect output")
                                                     

    (check-equal? 
   (contain-same-students?
    (list "Kathryn" "Amy") (list "Amy" "Kathryn" "hshsh" )) false
     "contain-same-students? returned incorrect output"))


;*******************************************************************

;; has-student? : Student ListofStudents -> boolean
;; GIVEN    : a Student and list of students in a roster.
;; RETURNS  : true iff the given list contains the student given.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF ormap on ListofStudents

(define (has-student? s1 lst)
  (ormap
         (lambda (s)
           (equal? s s1)) lst))

(begin-for-test
  (check-equal? 
   (has-student? "f" (list "Kathryn" "Amy")) false
   "has-student? returned other than expected output")
  (check-equal?
   (has-student? "Amy" (list "Kathryn" "Amy")) true
    "has-student? returned other than expected output"))

;*******************************************************************

;; getStudentsForClass : Class ListofEnrollmentRoster -> boolean
;; GIVEN    : a Class and list of enrollment roster.
;; RETURNS  : true iff the given list contains the student given.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF ormap on ListofEnrollmentRoster

(define (getStudentsForClass class erlist)
  (roster-students
   (first 
    (filter
     (lambda (r)
      (equal? class (roster-classname r))) erlist))))

(begin-for-test
  (check-equal? (getStudentsForClass "PDP"
                 (list
                  (make-roster "Netorks" (list "Kathryn" "Amy"))
                  (make-roster "PDP" (list "Feng" "Amy"  "John" ))))
                
                (list "Feng" "Amy" "John")
                 "getStudentsForClass returned other
                  than expected output"))

;*******************************************************************
;; BAD ENROLLMENTS TO ROSTERS


;; enrollments-to-rosters-bad-1 :
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN    : a set of enrollments
;; RETURNS  : a incorrect set of class rosters for the given enrollments
;; where there is one enrollment per student.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF map on SetOfEnrollmentAssertion
(define (enrollments-to-rosters-bad-1 ea)
  (if (> (length ea) 1)
  (map
      ; Class -> Roster
      ; RETURN : a roster for that class.
      (lambda (e)
       (make-roster (enrollment-class e) (list (enrollment-student e))))
       ea) '()))
;;;test
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-1
                 (list (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                 (list (make-roster "PDP" (list "John"))
                       (make-roster "Networks" (list "Kathryn"))
                       (make-roster "PDP" (list "Feng"))
                       (make-roster "PDP" (list "Amy"))
                       (make-roster "Networks" (list "Amy")))
                "enrollments-to-rosters-bad-1 returned other
                  than expected output"))

;*******************************************************************

;; enrollments-to-rosters-bad-2 :
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN    : a set of enrollments
;; RETURNS  : a incorrect set of class rosters for the given enrollments
;; where there is one less student always in all rosters
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF map on SetOfEnrollmentAssertion
(define (enrollments-to-rosters-bad-2 ea)
  (map
      ; Class -> Roster
      ; RETURN : a roster for that class.
      (lambda (c)
        (make-roster c
                      (rest(provide-students-for-class c ea))))
      (provide-class-set ea)))
  
;; test
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-2
                 (list 
                       (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                        (make-enrollment  empty "BMS")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                
                 (list (make-roster "PDP" (list "Feng" "Amy"))
                       (make-roster "Networks" (list "Amy"))
                       (make-roster "BMS" '()))
                  "enrollments-to-rosters-bad-2 returned other
                  than expected output"))

;*******************************************************************


;; enrollments-to-rosters-bad-3 :
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN    : a set of enrollments
;; RETURNS  : a incorrect set of class rosters for the given enrollments
;; where the first roster is always skipped.
;; EXAMPLES : See test cases
;; STRATEGY : Use HOF map on SetOfEnrollmentAssertion
(define (enrollments-to-rosters-bad-3 ea)
  (if (> (length ea) 1)
  (rest (map
      ; Class -> Roster
      ; RETURN : a roster for that class.
      (lambda (c)
       (make-roster c (provide-students-for-class c ea)))
        (provide-class-set ea))) '()))
  
;; test
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-3
                 (list 
                       (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                (list (make-roster "Networks" (list "Kathryn" "Amy")))
                 "enrollments-to-rosters-bad-3 returned other
                  than expected output"))


;**************************************END**************************************
