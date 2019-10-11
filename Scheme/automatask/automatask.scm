(define-macro (make-task prelude body fail #!optional (name "task") (doc ""))
  `(letrec ((self (lambda (#!optional (cmd #f))
                    (if (not cmd)
                        (if ,prelude
                            (call/cc (lambda (k) (if (,prelude)
                                                     (k (,body)))))
                            (call/cc (lambda (k) (k (,body)))))
                        (case cmd
                          ((prelude)
                           ,prelude)
                          ((body)
                           ,body)
                          ((fail)
                           ,fail)
                          ((name)
                           ,name)
                          ((doc)
                           ,doc))))))
     self))

(define _ #f)
(define _exec   (make-task _ (lambda () #t) _ "_exec" "Always execute"))
(define _stop   (make-task _ (lambda () #f) _ "_stop" "Stop execution"))
(define _oddsec (make-task _ (lambda () (odd (seconds))) _ "_oddsec" "Return true on odd seconds"))

(define _t1 (make-task _
                       (lambda () (begin (display (self 'doc))
                                         (newline)))
                       _
                       "_t1"
                       "Get doc string"))

(define _yoohoo! (make-task _
                            (lambda () (display "Yoohoo!")(newline))
                            _
                            "_yoohoo"
                            "Display \"Yoohoo!\""))

(define _boohoo! (make-task _
                            (lambda () ((self 'fail)))
                            (lambda () (display "Boohoo!")(newline))
                            "_boohoo"
                            "Display \"Boohoo!\" on failure"))

(define _boohoohoo! (make-task _
                               (lambda () (if (_oddsec)
                                              ((self 'fail))
                                              (pp "We are on an even second")))
                               _boohoo!
                               "_boohoohoo"
                               "Display \"evensec\" on even seconds else fails"))

(define (odd n) (= (modulo n 2) 1))
(define (even n) (= (modulo n 2) 0))
(define (seconds) (inexact->exact (floor (time->seconds (current-time)))))

(display "==================")(newline)
(display (_t1 'name))(newline)
(display (_t1 'doc))(newline)
(_t1)
(display "==================")(newline)(newline)

(define t2 (make-task _exec
                      _t1
                      _
                      "t2"
                      "Always execute _t1"))
(display "==================")(newline)
(display (t2 'name))(newline)
(display (t2 'doc))(newline)
(t2)
(display "==================")(newline)(newline)


(define t3 (make-task _oddsec
                      _yoohoo!
                      _
                      "t3"
                      "Execute _yoohoo! on odd seconds"))
(display "==================")(newline)
(display (t3 'name))(newline)
(display (t3 'doc))(newline)
(t3)
(display "==================")(newline)(newline)

(define t4 (make-task _oddsec
                      _boohoo!
                      _
                      "t4"
                      "Execute _boohoo! on odd seconds. _boohoo! always fails"))
(display "==================")(newline)
(display (t4 'name))(newline)
(display (t4 'doc))(newline)
(t4)
(display "==================")(newline)(newline)

(define t5 (make-task _exec
                      _boohoohoo!
                      (lambda () (display "Boohoohoo!")(newline))
                      "t5"
                      "Always execute _boohoohoo!, which fails on odd seconds"))
(display "==================")(newline)
(display (t5 'name))(newline)
(display (t5 'doc))(newline)
(t5)
(display "==================")(newline)


