#lang racket
(define c '(hello))
(require readline)
(require readline/rep-start)

;; basics

(define (lat? lat)
  (cond
    ((null? lat) #t)
    ((symbol? (car lat)) (lat? (cdr lat)))
    (else #f)))

(define (atom? a)
  (or (symbol? a) (number? a)))

(define (length lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat))))))

(define (pick lat n)
  (cond ((one? n) (car lat))
        (else (pick (cdr lat) (sub1 n)))))

(define (rempick lat n)
  (cond ((one? n) (cdr lat))
        (else (cons
               (car lat)
               (rempick (cdr lat) (sub1 n))))))

(define (member? lat a)
  (cond ((null? lat) #f)
        (else (or
               (eq? (car lat) a)
               (member? (cdr lat) a)))))

(define (rember lat a)
  (cond ((null? lat) '())
        ((eq? (car lat) a) (rember (cdr lat) a))
        (else (cons
               (car lat)
               (rember (cdr lat) a)))))

(define (firsts lat)
  (cond ((null? lat) '())
        (else
         (cons
          (car (car lat))
          (firsts (cdr lat))))))

(define (insertR lat old new)
  (cond ((null? lat) '())
        ((eq? (car lat) old)
         (cons
          old
          (cons
           new
           (insertR (cdr lat) old new))))
        (else (cons (car lat) (insertR (cdr lat) old new)))))


(define (insertL lat old new)
  (cond ((null? lat) '())
        ((eq? (car lat) old)
         (cons
          new
          (cons
           old
           (insertL (cdr lat) old new))))
        (else (cons (car lat) (insertL (cdr lat) old new)))))

(define (subst lat old new)
  (cond ((null? lat) '())
        ((eq? (car lat) old)
         (cons
          new
          (subst (cdr lat) old new)))
        (else (cons (car lat) (subst (cdr lat) old new)))))

(define (subst2 lat old1 old2 new)
  (cond ((null? lat) '())
        ((or (eq? old1 (car lat)) (eq? old2 (car lat)))
         (cons new (subst2 (cdr lat) old1 old2 new)))
        (else (cons (car lat) (subst2 (cdr lat) old1 old2 new)))))


(define (occur lat a)
  (cond ((null? lat) 0)
        ((eq? (car lat) a) (add1 (occur (cdr lat) a)))
        (else (occur (cdr lat) a))))

;; numbers

(define (+ x y)
    (cond ((zero? y) x)
          (else (add1 (+ x (sub1 y))))))

(define (- x y)
  (cond ((zero? y) x)
        (else (sub1 (- x (sub1 y))))))

(define (x x1 y)
  (cond ((zero? y) 0)
        (else (+ x1 (x x1 (sub1 y))))))

(define (> x y)
  (cond ((zero? x) #f)
        ((zero? y) #t)
        (else (> (sub1 x) (sub1 y)))))

(define (< x y)
  (cond ((zero? y) #f)
        ((zero? x) #t)
        (else (< (sub1 x) (sub1 y)))))

(define (= x y)
  (cond ((< x y) #f)
        ((> x y) #f)
        (else #t)))

(define (/ x y)
  (cond ((= x y) 1)
        ((< x y) 0)
        (else (add1 (/ (- x y) y)))))


(define (% x y)
  (cond ((= x y) 0)
        ((< x y) (- y x))
        (else (% (- x y) y))))

(define (^ x1 y)
  (cond ((zero? y) 1)
        (else (x x1 (^ x1 (sub1 y))))))

(define (powerlist_ x y)
  (cond ((zero? y) '())
        (else
         (cons (^ x y)
               (powerlist_ x (sub1 y))))))

(define (powerlist x y) (reverse (powerlist_ x y)))

(define (addtup tup)
  (cond ((null? tup) 0)
        (else (+ (car tup) (addtup (cdr tup))))))

(define (tup+ t1 t2)
  (cond
    ((null? t1) t2)
    ((null? t2) t1)
    (else (cons
           (+ (car t1) (car t2))
           (tup+ (cdr t1) (cdr t2))))))

(define (no-nums lat)
  (cond ((null? lat) '())
        ((number? (car lat)) (no-nums (cdr lat)))
        (else (cons
               (car lat)
               (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond ((null? lat) '())
        ((number? (car lat)) (cons
                              (car lat)
                              (all-nums (cdr lat))))
        (else (all-nums (cdr lat)))))

(define (one? x)
  (= x 1))

(define (leftmost lat)
  (cond ((null? lat) '())
        ((symbol? (car lat)) (car lat))
        (else (leftmost (car lat)))))

;; list recursive

(define (rember* lat a)
  (cond ((null? lat) '())
        ((atom? (car lat))
         (cond ((eq? (car lat) a) (rember* (cdr lat) a))
               (else (cons (car lat) (rember* (cdr lat) a)))))
        (else (cons (rember* (car lat) a) (rember* (cdr lat) a)))))

(define (occur* lat a)
  (cond ((null? lat) 0)
        ((symbol? (car lat))
         (cond ((eq? (car lat) a) (add1 (occur* (cdr lat) a )))
               (else (occur* (cdr lat) a))))
        (else (+ (occur* (car lat) a) (occur* (cdr lat) a)))))

(define (insertR* lat old new)
  (cond ((null? lat) '())
        ((symbol? (car lat))
         (cond ((eq? (car lat) old)
                (cons old (cons new (insertR* (cdr lat) old new))))
               (else (cons
                      (car lat)
                      (insertR* (cdr lat) old new)))))
        (else
         (cons
          (insertR* (car lat) old new)
          (insertR* (cdr lat) old new)))))

(define (insertL* lat old new)
  (cond ((null? lat) '())
        ((symbol? (car lat))
         (cond ((eq? (car lat) old)
                (cons old (cons new (insertL* (cdr lat) old new))))
               (else (cons
                      (car lat)
                      (insertL* (cdr lat) old new)))))
        (else
         (cons
          (insertL* (car lat) old new)
          (insertL* (cdr lat) old new)))))

(define (subst* lat a b)
  (cond ((null? lat) '())
        ((symbol? (car lat))
         (cond ((eq? (car lat) a) (cons b (subst* (cdr lat) a b)))
               (else (cons (car lat) (subst* (cdr lat) a b)))))
        (else (cons
               (subst* (car lat) a b)
               (subst* (cdr lat) a b)))))

(define (member* lat a)
  (cond ((null? lat) #f)
        ((symbol? (car lat))
         (or (eq? (car lat) a)
             (member* (cdr lat) a)))
        (else (or (member* (car lat) a) (member* (cdr lat) a)))))

;; equaling

(define (equan? x y)
  (cond ((and (number? x) (number? y)) (= x y))
        ((and (symbol? x) (symbol? y)) (eq? x y))
        (else #f)))

(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        (else (and
               (equal? (car l1) (car l2))
               (equal? (cdr l2) (cdr l2))))))

(define (equal? s1 s2)
  (cond ((and (atom? s1) (atom? s2)) (equan? s1 s2))
        ((or (atom? s1) (atom? s2)) #f)
        (else (eqlist? (cdr s1) (cdr s2)))))

(define (remberS lat s)
  (cond ((null? lat) '())
        ((equal? (car lat) s) (remberS (cdr lat) s))
        (else (cons (car lat) (remberS (cdr lat) s)))))

;; helpers

(define (numbered? s)
  (cond ((atom? s) (number? s))
        ((or (eq? (car s) 'x) (eq? (car s) '+) (eq? (car s) '-) (eq? (car s) '/) (eq? (car s) '^))
         (and (numbered? (car (cdr s)))
          (numbered? (car (cdr (cdr s))))))
        (else #f)))

(define (operator s)
  (car s))

(define (first-sub-exp s)
  (car (cdr s)))

(define (second-sub-exp s)
  (car (cdr (cdr s))))

(define (value s)
  (cond ((number? s) s)
        (else
         (cond
          ((eq? (operator s) '+)
           (+ (value (first-sub-exp s)) (value (second-sub-exp s))))
          ((eq? (operator s) '-)
           (- (value (first-sub-exp s)) (value (second-sub-exp s))))
          ((eq? (operator s) 'x)
           (x (value (first-sub-exp s)) (value (second-sub-exp s))))       
          ((eq? (operator s) '/)
           (/ (value (first-sub-exp s)) (value (second-sub-exp s))))
          ((eq? (operator s) '^)
           (^ (value (first-sub-exp s)) (value (second-sub-exp s))))))))

;; shadows

(define (new-zero lat)
  (null? lat))

(define (new-add1 lat)
  (cons '() lat))

(define (new-sub1 lat)
  (cdr lat))

(define (new-+ a b)
  (cond ((new-zero b) a)
        (else (new-add1 (new-+ a (new-sub1 b))))))

;; friends and relatives

(define (set? lat)
  (cond ((null? lat) #t)
        ((member? (cdr lat) (car lat)) #f)
        (else (set? (cdr lat)))))

(define (makeset lat)
  (cond ((null? lat) '())
        (else (cons
               (car lat)
               (makeset (rember (cdr lat) (car lat)))))))

(define (subset? s1 s2)
  (cond ((null? s2) #t)
        (else (and (member? s1 (car s2)) (subset? s1 (cdr s2))))))

(define (eqset? s1 s2)
  (and (eq? (length s1) (length s2)) (subset? s1 s2)))

(define (intersect? s1 s2)
  (cond ((null? s2) #f)
        (else (or (member? s1 (car s2)) (intersect? s1 (cdr s2))))))

(define (intersect s1 s2)
  (cond ((null? s2) '())
        ((member? s1 (car s2)) (cons (car s2) (intersect s1 (cdr s2))))
        (else (intersect s1 (cdr s2)))))

(define (union s1 s2)
  (cond ((null? s1) s2)
        ((member? s2 (car s1)) (union (cdr s1) s2))
        (else (cons (car s1) (union (cdr s1) s2)))))

(define (intersectall s)
  (cond ((null? (cdr s)) (car s))
        (else (intersect (car s) (intersectall (cdr s))))))

(define (a-pair p)
  (cond ((null? p) #f)
        ((atom? p) #f)
        ((null? (car p)) #f)
        ((null? (car (cdr p))) #f)
        ((null? (cdr (cdr p))) #t)
        (else #f)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (third p)
  (cdr (cdr p)))

(define (build a b)
  (cons a (cons b '())))

(define (fun? rel)
  (set? (firsts rel)))

(define (revrel rel)
  (cond ((null? rel) '())
        (else (cons
               (revpair (car rel))
               (revrel (cdr rel))))))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (one-to-one? rel)
  ((fun? (revrel rel))))

;; lambda the ultimate

(define (old-rember-f l a test?)
  (cond ((null? l) '())
        ((test? (car l) a) (old-rember-f (cdr l) a test?))
        (else (cons (car l) (old-rember-f (cdr l) a test?)))))

(define (eq?-c a)
  (lambda (x) (eq? x a)))

(define eq-salad
  (eq?-c 'salad))

(define (rember-f test?)
  (lambda (l a)
    (cond ((null? l) '())
          ((test? (car l) a) ((rember-f test?) (cdr l) a))
          (else (cons (car l) ((rember-f test?) (cdr l) a))))))

(define (insertL-f test?)
  (lambda (l old new)
    (cond ((null? l) '())
          ((test? (car l) old)
           (cons new (cons (car l) (cdr l))))
          (else
           (cons (car l) ((insertL-f test?) (cdr l) old new))))))

(define (insertR-f test?)
  (lambda (l old new)
    (cond ((null? l) '())
          ((test? (car l) old)
           (cons (car l) (cons new (cdr l))))
          (else
           (cons (car l) ((insertR-f test?) (cdr l) old new))))))

(define (seqL l old new)
  (cons new (cons old l)))

(define (seqR l old new)
  (cons old (cons new l)))

(define (seqS l old new)
  (cons new l))

(define (seqrem l old new)
  l)

(define (insert-g test? seq)
  (lambda (l old new)
    (cond ((null? l) '())
          ((test? (car l) old)
           (seq (cdr l) old new))
          (else
           (cons (car l) ((insert-g test? seq) (cdr l) old new))))))


(define (insert-g-new test? seq)
  (lambda (l old new)
    (cond ((null? l) '())
          ((test? (car l) old)
           (seq old new ((insert-g-new test? seq) (cdr l) old new)))
          (else
           (cons (car l) ((insert-g-new test? seq) (cdr l) old new))))))

(define rember-new (lambda (l a) ((insert-g equal? seqrem) l a #f)))
(define insertL-new (insert-g equal? seqL))
(define insertR-new (insert-g equal? seqR))

(define (atom-to-function x)
  (cond
    ((eq? x '+) +)
    ((eq? x '-) -)
    ((eq? x 'x) x)    
    (else /)))

(define (value-new s op)
  (cond ((number? s) s)
        (else
         ((op (operator s))
          (value (first-sub-exp s))
          (value (second-sub-exp s))))))

(define (multirember&co lat a col)
  (cond ((null? lat) (col '() '()))
        ((eq? (car lat) a)
         (multirember&co (cdr lat) a
                           (lambda (newval seen)
                             (col newval
                                  (cons (car lat) seen)))))
        (else
         (multirember&co (cdr lat) a
                         (lambda (newval seen)
                           (col (cons (car lat) newval)
                                seen))))))

(define (multiinsertLR lat oldL oldR new)
  (cond ((null? lat) '())
        ((eq? (car lat) oldL)
         (cons new (cons oldL (multiinsertLR (cdr lat) oldL oldR new))))
        ((eq? (car lat) oldR)
         (cons oldR (cons new (multiinsertLR (cdr lat) oldL oldR new))))
        (else
         (cons (car lat) (multiinsertLR (cdr lat) oldL oldR new)))))

(define (multiinsertLR&co lat oldL oldR new col)
  (cond ((null? lat) (col '() 0 0))
        ((eq? (car lat) oldR)
         (multiinsertLR&co (cdr lat) oldL oldR new
                           (lambda (newlat L R)
                             (col (cons oldR (cons new newlat)) (add1 R) L))))
        ((eq? (car lat) oldL)
         (multiinsertLR&co (cdr lat) oldL oldR new
                           (lambda (newlat L R)
                             (col (cons new (cons oldL newlat)) R (add1 L)))))
        (else
         (multiinsertLR&co (cdr lat) oldL oldR new
                           (lambda (newlat L R)
                             (col (cons (car lat) newlat) R L))))))

(define (even? n)
  (= (% n 2) 0))

(define (evens-only* sexp)
  (cond ((null? sexp) '())
        ((atom? (car sexp))
         (cond ((even? (car sexp))
                (cons (car sexp) (evens-only* (cdr sexp))))
               (else
                (evens-only* (cdr sexp)))))
        (else
         (cons (evens-only* (car sexp)) (evens-only* (cdr sexp))))))

(define (evens-only*&co sexp col)
  (cond ((null? sexp) (col '() 1 0))
        ((atom? (car sexp))
         (cond ((even? (car sexp))
                 (evens-only*&co
                  (cdr sexp)
                  (lambda (evens evens-product odd-sum)
                    (col (cons (car sexp) evens)
                         (x (car sexp) evens-product)
                         odd-sum))))
                (else
                 (evens-only*&co
                  (cdr sexp)
                  (lambda (evens evens-product odd-sum)
                    (col evens
                         evens-product
                         (+ (car sexp) odd-sum)))))))
        (else
         (evens-only*&co (car sexp)
                         (lambda (evens evens-product odd-sum)
                           (evens-only*&co
                            (cdr sexp)
                            (lambda (evens1 evens-product1 odd-sum1)
                              (col (prepend evens1 evens)
                                   (x evens-product1 evens-product)
                                   (+ odd-sum odd-sum1)))))))))

;; and again

(define (shift x)
  (build (first (first x))
         (build (second (first x)) (second x))))

(define (align pora)
  (cond
    ((atom? pora) pora)
    ((a-pair (first pora))
     (align (shift pora)))
    (else (build
           (first pora)
           (align (second pora))))))
    
(define (eternity x)
  (eternity x))

(lambda (l)
  (cond ((null? l) 0)
        (else (add1
               ((lambda (l)
                 (cond ((null? l) 0)
                       (else (add1
                              ((lambda (l)
                                (cond ((null? l) 0)
                                      (else (eternity l))))
                              (cdr l))))))
               (cdr l))))))

((lambda (len)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (len (cdr l)))))))
 eternity)


((lambda (mk-len)
  (mk-len eternity))
 (lambda (len)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (len (cdr l))))))))

;; OTHER

(define (prepend one two)
    (cond ((null? two) one)
          (else 
           (cons (car two) (prepend one (cdr two))))))

(define (reverse_numbers n)
  (cond ((eq? n 0) '())
        (else (cons n (reverse_numbers (sub1 n))))))

(define (reverse_num lat seq)
  (cond ((null? seq) '())
        (else (cons
               (pick lat (sub1 (car seq)))
               (reverse_num lat (cdr seq))))))

(define (reverse lat)
  (letrec ((len (length lat))
           (seq (reverse_numbers len)))
    (reverse_num lat seq)))
