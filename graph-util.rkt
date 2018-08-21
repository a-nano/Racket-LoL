#lang racket
(require srfi/1   ; pair-for-each
         srfi/13) ; string-map string-take

(provide ugraph->png) ; for wumpus.rkt

(define *wizard-nodes* '((living-room
                          (you are in the living-room.
                               a wizard is snoring loudly on the couch.))
                         (garden
                          (you are in a beautiful garden.
                               there is a well in front of you.))
                         (attic
                          (you are in the attic.
                               there is a giant welding torch in the corner.))))

(define *wizard-edges* '((living-room (garden west door)
                                      (attic upstairs ladder))
                         (garden (living-room east door))
                         (attic (living-room downstairs ladder))))

(define (dot-name exp)
  (string-map
   (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)) c #\_))
   (call-with-output-string (lambda (out) (write exp out)))))

(define *max-label-length* 30)

(define (dot-label exp)
  (if (null? exp)
      ""
      (let ([s (call-with-output-string
                (lambda (out) (write exp out)))])
        (if (> (string-length s) *max-label-length*)
            (string-append (string-take s (- *max-label-length* 3)) "...")
            s))))

(define (nodes->dot nodes)
  (for-each (lambda (node)
              (newline)
              (display (dot-name (car node)))
              (display "[label=\"")
              (display (dot-label node))
              (display "\"];"))
            nodes))

(define (edges->dot edges)
  (for-each (lambda (node)
              (for-each (lambda (edge)
                          (newline)
                          (display (dot-name (car node)))
                          (display "->")
                          (display (dot-name (car edge)))
                          (display "[label=\"")
                          (display (dot-label (cdr edge)))
                          (display "\"];"))
                        (cdr node)))
            edges))

(define (graph->dot nodes edges)
  (display "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (display "}"))

(define (dot->png fname thunk)
  (let ([fn (string-append fname ".dot")])
    (with-output-to-file
        fn
      thunk
      #:exists 'replace)
    (system (string-append "dot -Tpng -O " fn))))

(define (graph->png fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(define (uedges->dot edges)
  (pair-for-each (lambda (lst)
                   (for-each (lambda (edge)
                               (unless (assv (car edge) (cdr lst))
                                 (newline)
                                 (display (dot-name (caar lst)))
                                 (display "--")
                                 (display (dot-name (car edge)))
                                 (display "[label=\"")
                                 (display (dot-label (cdr edge)))
                                 (display "\"];")))
                             (cdar lst)))
                 edges))

(define (ugraph->dot nodes edges)
  (display "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (display "}"))

(define (ugraph->png fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))