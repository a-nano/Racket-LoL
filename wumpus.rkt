#lang racket
(require srfi/1) ; lset-difference delete-duplicates
(require br/list) ; push! from mbutterick/beautiful-racket
(require "./graph-util.rkt")


(define *congestion-city-nodes* '())
(define *congestion-city-edges* '())
(define *visited-nodes* '())
(define *node-num* 30)
(define *edge-num* 45)
(define *worm-num* 3)
(define *cop-odds* 15)
(define *player-pos* 1)

(define (random-node)
  (+ 1 (random *node-num*)))

(define (edge-pair a b)
  (if (eqv? a b)
      '()
      (list (cons a b) (cons b a))))

(define (make-edge-list)
  (apply append (for/list ([i *edge-num*])
                  (edge-pair (random-node) (random-node)))))


(define (direct-edges node edge-list)
  (filter (lambda (x) (eqv? (car x) node)) edge-list))

(define (get-connected node edge-list)
  (let ([visited '()])
    (define (traverse node)
      (unless (member node visited)
        (push! visited node)
        (for-each (lambda (edge)
                    (traverse (cdr edge)))
                  (direct-edges node edge-list))))
    (traverse node)
    visited))

(define (find-islands nodes edge-list)
  (let ([islands '()])
    (define (find-island nodes)
      (when (pair? nodes)
        (let* ([connected (get-connected (car nodes) edge-list)]
               [unconnected (lset-difference eqv? nodes connected)])
          (push! islands connected)
          (when unconnected
            (find-island unconnected)))))
    (find-island nodes)
    islands))

(define (connect-with-bridges islands)
  (if (null? (cdr islands))
      '()
      (append (edge-pair (caar islands) (caadr islands))
              (connect-with-bridges (cdr islands)))))

(define (connect-all-islands nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(define (make-city-edges)
  (let* ([nodes (iota *node-num* 1)]
         [edge-list (connect-all-islands nodes (make-edge-list))]
         [cops (filter (lambda (x)
                         (zero? (random *cop-odds*)))
                       edge-list)])
    (add-cops (edges-to-alist edge-list) cops)))

(define (edges-to-alist edge-list)
  (map (lambda (node1)
         (cons node1
               (map (lambda (edge)
                      (list (cdr edge)))
                    (delete-duplicates (direct-edges node1 edge-list) equal?))))
       (delete-duplicates (map car edge-list))))

(define (add-cops edge-alist edges-with-cops)
  (map (lambda (x)
         (let ([node1 (car x)]
               [node1-edges (cdr x)])
           (cons node1
                 (map (lambda (edge)
                        (let ([node2 (car edge)])
                          (if (null? (lset-intersection equal?
                                                        (edge-pair node1 node2)
                                                        edges-with-cops))
                              edge
                              (list node2 'cops))))
                      node1-edges))))
       edge-alist))

(define (neighbors node edge-alist)
  (map car (cdr (assoc node edge-alist))))

(define (within-one a b edge-alist)
  (member b (neighbors a edge-alist)))

(define (within-two a b edge-alist)
  (or (within-one a b edge-alist)
      (any (lambda (x)
             (within-one x b edge-alist))
           (neighbors a edge-alist))))

(define (make-city-nodes edge-alist)
  (let ([wumpus (random-node)]
        [glow-worms (for/list ([i *worm-num*]) (random-node))])
    (for/list ([n (iota *node-num* 1)])
      (append (list n)
              (cond [(eqv? n wumpus) '(wumpus)]
                    [(within-two n wumpus edge-alist) '(blood!)]
                    [else '()])
              (cond [(memv n glow-worms) '(glow-worm)]
                    [(any (lambda (worm)
                            (within-one n worm edge-alist))
                          glow-worms)
                     '(lights!)]
                    [else '()])
              (if (empty? (any cdr (cdr (assoc n edge-alist))))
                  '()
                '(sirens!))))))

#|
(define (new-game)
  (set! *congestion-city-edges* (make-city-edges))
  (set! *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (set! *player-pos* (find-empty-node))
  (set! *visited-nodes* (list *player-pos*))
  (draw-city))
|#

(define (find-empty-node)
  (let ([empty-nodes (filter (lambda (n) (null? (cdr n)))
                             *congestion-city-nodes*)])
    (when (null? empty-nodes)
      (error "City is too congested. Try (new-game) again."))
    (car (list-ref empty-nodes (random (length empty-nodes))))))
    
(define (draw-city)
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(define (known-city-nodes)
  (map (lambda (node)
         (if (memv node *visited-nodes*)
             (let ([n (assv node *congestion-city-nodes*)])
               (if (eqv? node *player-pos*)
                   (append n '(*))
                   n))
             (list node '?)))
       (delete-duplicates
        (append *visited-nodes*
                (append-map (lambda (node)
                              (map car
                                   (cdr (assoc node
                                               *congestion-city-edges*))))
                            *visited-nodes*)))))

(define (known-city-edges)
  (map (lambda (node)
         (cons node (map (lambda (x)
                           (if (member (car x) *visited-nodes*)
                               x
                               (list (car x))))
                         (cdr (assoc node *congestion-city-edges*)))))
       *visited-nodes*))

(define (draw-known-city)
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(define (new-game)
  (set! *congestion-city-edges* (make-city-edges))
  (set! *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (set! *player-pos* (find-empty-node))
  (set! *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(define (walk pos)
  (handle-direction pos #f))

(define (charge pos)
  (handle-direction pos #t))

(define (handle-direction pos charging)
  (let ([edge (assv pos
                    (cdr (assv *player-pos* *congestion-city-edges*)))])
    (if edge
        (handle-new-place edge pos charging)
        (display "That location does not exist!"))))


(define (handle-new-place edge pos charging)
  (let* ([node (assv pos *congestion-city-nodes*)]
         [has-worm (and (memv 'glow-worm node)
                        (not (memv pos *visited-nodes*)))])
    (unless (memv pos *visited-nodes*) (push! *visited-nodes* pos))
    (set! *player-pos* pos)
    (draw-known-city)
    (cond [(memq 'cops edge) (display "You ran into the cops. Game Over.")]
          [(memq 'wumpus node) (if charging
                                   (display "you found the Wumpus!")
                                   (display "You ran into th Wumpus"))]
          [charging (display "You wasted your last bullet. Game Over.")]
          [has-worm (let ([new-pos (random-node)])
                      (display "You ran into Glow Worm Gang! You're now at ")
                      (display new-pos)
                      (handle-new-place '() new-pos #f))])))

