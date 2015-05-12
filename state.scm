;;;;=========================================================================
;;;; File   : state.scm
;;;; Author : Dylan Gleason
;;;;
;;;; State "class" represents a State in a FSM graph. It has both "a"
;;;; and "b" transitions, for regular languages composed of characters
;;;; "a" and "b".
;;;;=========================================================================

(define State
  (lambda (name)
    (let ((is-final #f) (a-trans '()) (b-trans '()))
      (letrec
          ;; accessor for name field
          ((get-name
            (lambda () name))

           ;; accessor for is-final field
           (get-final
            (lambda () is-final))

           ;; accessor for the a-transition
           (get-a-trans
            (lambda () a-trans))

           ;; accessor for the b-transition
           (get-b-trans
            (lambda () b-trans))

           ;; mutator for the name field
           (set-name!
            (lambda (new-name)
              (set! name new-name) name))

           ;; mutator for the is-final field
           (set-final!
            (lambda (new-final)
              (set! is-final new-final) is-final))

           ;; mutator for the a-trans field
           (set-a-trans!
            (lambda (new-a-trans)
              (set! a-trans new-a-trans) a-trans))

           ;; mutator for the b-trans field
           (set-b-trans!
            (lambda (new-b-trans)
              (set! b-trans new-b-trans) b-trans))

           ;; `test-string` procedure will test each character in the
           ;; string and then recur on the function until the string
           ;; is empty.
           (test-string
            (lambda (expr)
              (cond
               ((= 0 (string-length expr)) is-final)
               ((equal? (substring expr 0 1) "a")
                ((a-trans 'test-string)
                 (substring expr 1 (string-length expr))))
               ((equal? (substring expr 0 1) "b")
                ((b-trans 'test-string)
                 (substring expr 1 (string-length expr))))
               (else #f))))

           ;; `dispatch' procedure will forward the given message to
           ;; the correct "method" above.
           (dispatch
            (lambda (message)
              (case message
                ((get-name) get-name)
                ((get-final) get-final)
                ((get-a-trans) get-a-trans)
                ((get-b-trans) get-b-trans)
                ((set-name!) set-name!)
                ((set-final!) set-final!)
                ((set-a-trans!) set-a-trans!)
                ((set-b-trans!) set-b-trans!)
                ((test-string) test-string)
                (else (error 'self "INVALID MESSAGE" message))))))
        dispatch))))


;; Definitions which convert message passing syntax into "method
;; call" syntax.

(define get-name
  (lambda (obj)
    ((obj 'get-name))))

(define get-final
  (lambda (obj)
    ((obj 'get-final))))

(define get-a-trans
  (lambda (obj)
    ((obj 'get-a-trans))))

(define get-b-trans
  (lambda (obj)
    ((obj 'get-b-trans))))

(define set-name!
  (lambda (obj new-val)
    ((obj 'set-name!) new-val)))

(define set-final!
  (lambda (obj new-val)
    ((obj 'set-final!) new-val)))

(define set-a-trans!
  (lambda (obj new-val)
    ((obj 'set-a-trans!) new-val)))

(define set-b-trans!
  (lambda (obj new-val)
    ((obj 'set-b-trans!) new-val)))

(define test-string
  (lambda (obj str)
    ((obj 'test-string) str)))
