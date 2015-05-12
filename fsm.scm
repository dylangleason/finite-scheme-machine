;;;;=========================================================================
;;;; File   : fsm.scm
;;;; Author : Dylan Gleason
;;;;
;;;; FSM represents a Finite State Machine that will be constructed
;;;; after reading an input file. Once the Finite State Machine graph
;;;; is constructed it will then determine whether each string in the
;;;; input file is valid for the constructed Finite State Machine.
;;;;=========================================================================

(load "state.scm")    ; FSM depends on State definitions

(define FSM
  (lambda ()
    (let ((i-state '()) (state-map (make-hashtable string-hash string=?)))
      (letrec          
          (;; build-map function reads input file and creates a map of
           ;; states.  It then will test each string in the input file
           ;; and will determine if each input string is a valid
           ;; regular expression for the given FSM
           (build-map
            (lambda (file)
              (let ((input (open-input-file file)))
                (begin
                  (if (not (= 0 (hashtable-size state-map)))
                      (hashtable-clear! state-map))
                  (call/cc
                   (lambda (return)
                     (let loop ((count 0))
                       (let ((str (get-line input)))
                         (begin
                           (if (> count 1)
                               (if (string=? (substring str 0 1) "F")
                                   (begin
                                     (display str)
                                     (newline)
                                     (process-final-states str)
                                     (return))
                                   (let ((lines (string-split str "," '())))
                                     (begin
                                       (display str)
                                       (newline)
                                       (process-states lines count)))))
                           (loop (+ count 1))))))))
                (call/cc
                 (lambda (return)
                   (let loop ()
                     (let ((str (get-line input)))
                       (begin
                         (if (eof-object? str)
                             (return)
                             (begin
                               (newline)
                               (display str)
                               (newline)
                               (if (test-string i-state str)
                                   (begin
                                     (display "ACCEPT")
                                     (newline))
                                   (begin
                                     (display "REJECT")
                                     (newline)))))
                         (loop)))))))))

           ;; process-states function will process all of the states
           ;; in the file one by one and create the state-map graph
           (process-states
            (lambda (lines count)
              (let ((current-state (caddr lines))
                    (transition (cadr lines))
                    (next-state (car lines)))
                (begin (if (= count 2)
                           (begin (set! i-state (State current-state))
                                  (hashtable-set! state-map current-state i-state)))
                       (if (not (hashtable-contains? state-map current-state))
                           (hashtable-set! state-map current-state
                                           (State current-state)))
                       (if (not (hashtable-contains? state-map next-state))
                           (hashtable-set! state-map next-state (State next-state)))
                       (if (string=? transition "a")
                           (set-a-trans! (hashtable-ref state-map current-state '())
                                         (hashtable-ref state-map next-state '()))
                           (set-b-trans! (hashtable-ref state-map current-state '())
                                         (hashtable-ref state-map
                                                        next-state '())))))))

           ;; process-final-states function will process all states
           ;; that are designated as final states in the input file,
           ;; i.e. when it processes a final state the function will
           ;; look for the corresponding state in the hashtable and
           ;; set its is-final field to #t.
           (process-final-states
            (lambda (str)
              (let ((final-states
                     (string-split (substring str 3 (string-length str)) "," '())))
                (call/cc
                 (lambda (return)
                   (let loop ((lst final-states))
                     (if (null? lst)
                         (return)
                         (begin
                           (if (not (hashtable-contains? state-map (car lst)))
                               (hashtable-set! state-map (car lst)
                                               (State (car lst))))
                           (set-final! (hashtable-ref state-map (car lst) '()) #t)
                           (loop (cdr lst))))))))))

           ;; `string-split' will split the string according to the
           ;; delimiter and return a list of characters representing
           ;; (in order) the next-state, state transition, and the
           ;; current-state.
           (string-split
            (lambda (str delim lst)
              (cond ((= 0 (string-length str)) lst)
                    ((= 1 (string-length str))
                     (string-split (substring str 1 (string-length str))
                                   delim
                                   (cons (substring str 0 1) lst)))
                    ((string=? delim (substring str 1 2))
                     (string-split (substring str 1 (string-length str))
                                   delim
                                   (cons (substring str 0 1) lst)))
                    (else
                     (string-split (substring str 1 (string-length str))
                                   delim
                                   lst)))))

           ;; Function will dispatch the message argument to the
           ;; appropriate above "method" in the FSM. In this case,
           ;; only the `build-map' "method" is exposed while all
           ;; others are "private".
           (dispatch
            (lambda (message)
              (case message
                ((build-map) build-map)))))
        dispatch))))

;; build-map wraps message-passing style syntax into "method-call"
;; style syntax.

(define build-map
  (lambda (obj file)
    ((obj 'build-map) file)))
