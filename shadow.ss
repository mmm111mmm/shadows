#lang racket

(define c '(hello))
(require readline)
(require readline/rep-start)
(require 2htdp/universe)
(require 2htdp/image)

;; basics

(define (lat? lat)
  (cond
    ((null? lat) #t)
    ((symbol? (car lat)) (lat? (cdr lat)))
    (else #f)))

(define (atom? a)
  (or (symbol? a) (number? a) (eq? a #f) (eq? a #t)))

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

;(define (+ x y)
 ;   (cond ((zero? y) x)
  ;        (else (add1 (+ x (sub1 y))))))

;(define (- x y)
 ; (cond ((zero? y) x)
  ;      (else (sub1 (- x (sub1 y))))))

;(define (x x1 y)
 ; (cond ((zero? y) 0)
  ;      (else (+ x1 (x x1 (sub1 y))))))

;(define (> x y)
;  (cond ((zero? x) #f)
 ;       ((zero? y) #t)
  ;      (else (> (sub1 x) (sub1 y)))))

;(define (< x y)
 ; (cond ((zero? y) #f)
  ;      ((zero? x) #t)
   ;     (else (< (sub1 x) (sub1 y)))))

;(define (= x y)
 ; (cond ((< x y) #f)
  ;      ((> x y) #f)
   ;     (else #t)))

;(define (/ x y)
 ; (cond ((= x y) 1)
  ;      ((< x y) 0)
   ;     (else (add1 (/ (- x y) y)))))


;(define (% x y)
 ; (cond ((= x y) 0)
  ;      ((< x y) (- y x))
   ;     (else (% (- x y) y))))

;(define (^ x1 y)
 ; (cond ((zero? y) 1)
  ;      (else (x x1 (^ x1 (sub1 y))))))

;(define (powerlist_ x y)
 ; (cond ((zero? y) '())
  ;      (else
   ;      (cons (^ x y)
    ;           (powerlist_ x (sub1 y))))))

;(define (powerlist x y) (reverse (powerlist_ x y)))

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
               (equal? (cdr l1) (cdr l2))))))



(define (equal? s1 s2)
  (cond ((and (atom? s1) (atom? s2)) (equan? s1 s2))
        ((or (atom? s1) (atom? s2)) #f)
        (else (eqlist? s1 s2))))

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
           (* (value (first-sub-exp s)) (value (second-sub-exp s))))       
          ((eq? (operator s) '/)
           (/ (value (first-sub-exp s)) (value (second-sub-exp s))))
          ((eq? (operator s) '^)
           (expt (value (first-sub-exp s)) (value (second-sub-exp s))))))))

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
          (value-new (first-sub-exp s) op)
          (value-new (second-sub-exp s) op)))))

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
  (= (quotient n 2) 0))

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
                         (* (car sexp) evens-product)
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
                                   (* evens-product1 evens-product)
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

((lambda (l)
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
 '(a b))

(
 ((lambda (len)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (len (cdr l)))))))
  eternity
 )
 '()
)


(
 ((lambda (mk-len)
   (mk-len
    (mk-len
     (mk-len
      (mk-len eternity)))))
  (lambda (len)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (len (cdr l)))))))
 )
 '(a b c)
)

(
 ((lambda (mk-len)
   (mk-len mk-len))
  (lambda (mk-len)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 ((mk-len mk-len) (cdr l)))))))
 )
 '(a b c d e f g h i j k l m)
)


(
 ((lambda (mk-len) (mk-len mk-len))
  (lambda (mk-len)
    ((lambda (len)
       (lambda (l)
         (cond ((null? l) 0)
            (else (add1 (len (cdr l)))))))
     (lambda (x) ((mk-len mk-len) x)))))
 '(a b c d e f g h i j k l m)
)

;; Chapter: What is the value

;; tables and environments

(define new-entry build)

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-helper name (first entry) (second entry) entry-f))

(define (lookup-in-entry-helper name names values entry-f)
  (cond ((null? names) (entry-f name))
        ((eq? name (car names)) (car values))
        (else (lookup-in-entry-helper name (cdr names) (cdr values) entry-f))))

(define extend-table cons)

(define (lookup-in-table name table table-f)
  (cond ((null? table) (table-f name))
        (else (lookup-in-entry name
                               (car table)
                               (lambda (name)
                                 (lookup-in-table name (cdr table) table-f))))))
;; get meaning and value

(define (val e)
  (expression-to-action e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (expression-to-action e)
  (cond ((atom? e) (atom-to-action e))
        (else (list-to-action e))))

;; types of expressions

(define (atom-to-action atom)
  (cond ((number? atom) *const)
        ((eq? atom 'cons) *const)
        ((eq? atom 'cdr) *const)
        ((eq? atom 'null?) *const)
        ((eq? atom 'add1) *const)
        ((eq? atom 'sub1) *const)
        ((eq? atom 'zero?) *const)
        ((eq? atom 'number?) *const)
        ((eq? atom 'eq?) *const)
        ((eq? atom 'atom?) *const)
        ((eq? atom #f) *const)
        ((eq? atom #t) *const)
        (else *identifier)))

(define (list-to-action l)
  (cond ((atom? (car l))
         (cond ((eq? 'lambda (car l))
                *lambda)
               ((eq? 'cond (car l))
                *cond)
               ((eq? 'quote (car l))
                *quote)
               (else
                *application)))
        (else *application)))

;; application - primative

(define function-of car)
(define arguments-of cdr)

(define (*application e table)
  (apply
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (evlis args table)
  (cond ((null? args) '())
        (else
         (cons (meaning (car args) table) (evlis (cdr args) table)))))

(define (primative? e)
  (eq? (first e) 'primative))

(define (non-primative? e)
  (eq? (first e) 'non-primative))

(define (apply fun args)
  (cond ((primative? fun)
         (apply-primative (second fun) args))
        ((non-primative? fun)
         (apply-closure (second fun) args))))

(define (apply-primative fun args)
  (cond ((eq? fun 'car)
         (car (first args)))
        ((eq? fun 'cdr)
         (cdr (first args)))
        ((eq? fun 'cons)
         (cons (first args) (second args)))))
        

;; lambda

(define (*lambda e table)
  (build 'non-primative (cons table (cdr e))))

;; application - non primative / lambda

(define (body-of e)
  (car (cdr (cdr e))))

(define (formals-of e)
  (car (cdr e)))

(define (table-of e)
  (car e))

(define (apply-closure closure args)
  (meaning (body-of closure)
           (extend-table
            (new-entry
             (formals-of closure)
             args)
            (table-of closure))))

;; const

(define (*const e table)
  (cond ((number? e) e)
        ((eq? '#f e) #f)
        ((eq? '#t e) #t)
        (else (build 'primative e))))

;; identifier

(define (initial-table name)
  (car '()))

(define (*identifier e table)
  (lookup-in-table e table initial-table))

;; quote

(define (text-of e)
  (car (cdr e)))

(define (*quote e table)(
  (text-of e)))

;; cond

(define question-of first)
(define answer-of second)
(define cond-lines-of cdr)
(define (else? e)
  (cond ((atom? (question-of e)) (eq? (question-of e) 'else))
        (else #f)))

(define (evcon lines table)
  (cond ((else? (question-of (car lines)))
         (meaning (answer-of (car lines)) table))
        ((meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table))
        (else (evcon (cdr lines) table))))

(define (*cond e table)
  (evcon (cond-lines-of e) table))

;; the seasoned schemer

(define (two-in-a-row? lat)
  (cond ((null? lat) #f)
        (else (or (two-in-a-row-helper (car lat) (cdr lat))
                  (two-in-a-row? (cdr lat))))))

(define (two-in-a-row-helper l lat)
  (cond ((null? lat) #f)
        (else (eq? l (car lat)))))

(two-in-a-row? '(one one))

(define (two-in-a-row-two? lat)
  (cond ((null? lat) #f)
        (else (two-in-a-row-two-helper (car lat) (cdr lat)))))

(define (two-in-a-row-two-helper l lat)
  (cond ((null? lat) #f)
        (else (or (eq? l (car lat)) (two-in-a-row-two? (cdr lat))))))

(two-in-a-row-two? '(two two))

(define (two-in-a-row-three? lat)
  (cond ((null? lat) #f)
        (else (two-in-a-row-three-helper (car lat) (cdr lat)))))

(define (two-in-a-row-three-helper preceding lat)
  (cond ((null? lat) #f)
        (else (or (eq? preceding (car lat)) (two-in-a-row-three-helper (car lat) (cdr lat))))))

(two-in-a-row-two? '(three three))

(define (sum-of-prefixes preceding lat)
  (cond ((null? lat) '())
        (else (cons (+ (car lat) preceding) (sum-of-prefixes (+ (car lat) preceding) (cdr lat))))))

(sum-of-prefixes 0 '(1 1 1))

;; hop skip jump

(define (member-new? lat a)
  (cond ((null? lat) #f)
        (else (or
               (eq? (car lat) a)
               (member-new? (cdr lat) a)))))

(define (intersect-new s1 s2)
  (letrec ((isect (lambda (s)
                        (cond ((null? s) '())
                              ((member-new? s1 (car s)) (cons (car s) (isect (cdr s))))
                              (else (isect (cdr s)))))))
    (isect s2)))

(intersect-new '(a b c) '(a z z))

(define (intersect-all-new lset)
  (letrec ((isetall (lambda (lset)
                    (cond ((null? (cdr lset)) (car lset))
                          (else (intersect-new (car lset)
                                               (isetall (cdr lset))))))))
    (cond ((null? lset) '())
          (else (isetall lset)))))

(intersect-all-new '((a z c)(a e z)) )

(define (intersect-all-cc lset)
  (call-with-current-continuation
   (lambda (hop)
     (letrec
         ((isetall (lambda (lset)
                     (cond
                       ((null? (car lset)) (hop '()))
                       ((null? (cdr lset)) (car lset))   
                       (else (intersect-new (car lset)
                                            (isetall (cdr lset))))))))
       (cond ((null? lset) '())
             (else (isetall lset)))))))

(intersect-all-cc '((y z c)(a e y)) )

(define (intersect-all-cc-two lset)
  (call-with-current-continuation
   (lambda (hop)
     (letrec
         ((intersect (lambda (s1 s2)
                       (letrec ((isect (lambda (s)
                                         (cond ((null? s) '())
                                               ((member-new? s2 (car s)) (cons (car s) (isect (cdr s))))
                                               (else (isect (cdr s)))))))
                         (cond ((null? s2) (hop '(none)))
                               (else (isect s1))))))
          (isetall (lambda (lset)
                     (cond
                       ((null? (car lset)) (hop '(empty-set)))
                       ((null? (cdr lset)) (car lset))   
                       (else (intersect (car lset)
                                        (isetall (cdr lset))))))))
       (cond ((null? lset) '())
             (else (isetall lset)))))))

(intersect-all-cc-two '((y z c)()(a e p)))
(intersect-all-cc-two '((y z c)(i d j)(a e p)))

(define (rember-rec atom lat)
  (letrec ((R (lambda (lat)  
                (cond ((null? lat) '())
                      ((eq? atom (car lat)) (cdr lat))
                      (else (cons (car lat) (R (cdr lat))))))))
    (R lat)))

(rember-rec 'a '(b a c))

(define (rember-beyond-first atom lat)
  (letrec ((R (lambda (lat)
                (cond ((null? lat) '())
                      ((eq? atom (car lat)) '())
                      (else (cons (car lat) (R (cdr lat))))))))
    (R lat)))

(rember-beyond-first 'c '(b a c d g))
(rember-beyond-first 'z '(b a c d g))

(define (rember-upto-last atom lat)
  (call-with-current-continuation
   (lambda (skip)
     (letrec ((R (lambda (lat)
                   (cond ((null? lat) '())
                         ((eq? atom (car lat)) (skip (R (cdr lat))))
                         (else (cons (car lat) (R (cdr lat))))))))
       (R lat)))))

(rember-upto-last 'c '(a b c d e))
(rember-upto-last 'c '(a b c d e c z x))
(rember-upto-last 'z '(a b c d e))

;; Let there be names

(define (leftmost-new lat)
  (cond ((null? lat) '())
        ((atom? (car lat)) (car lat))
        (else
         (let ((leftmost-car (leftmost-new (car lat))))
           (cond ((atom? leftmost-car) leftmost-car)
                 (else
                  (leftmost-new (cdr lat))))))))

(leftmost-new '(a b c))
(leftmost-new '(((a)) b c))
(leftmost-new '((() b) c))
(leftmost-new '((()()) c))

(define (rember1* atom l)
  (letrec ((R (lambda (l)
                (cond ((null? l) '())
                      ((atom? (car l))
                       (cond ((eq? atom (car l))
                              (cdr l))
                             (else (cons (car l) (R (cdr l))))))
                      (else
                       (let ((R-car (R (car l))))
                         (cond ((eqlist? (car l) R-car)
                                (cons R-car (R (cdr l))))
                               (else (cons R-car (cdr l))))))))))
    (R l)))

(rember1* 'b '(a (b c) d))
(rember1* 'c '(a (b c) d))

(define (depth* l)
  (cond ((null? l) 1)
        ((atom? (car l))
         (depth* (cdr l)))
        (else
         (let ((depth-car (add1 (depth* (car l))))
               (depth-cdr (depth* (cdr l))))
           (cond
             ((> depth-car depth-cdr) depth-car)
             (else depth-cdr))))))

(depth* '(a a) )
(depth* '(a (a) a) )
(depth* '((pickled)
          peppers
          (peppers pickled)))
(depth* '(()
          ((bitter butter)
           (makes)
           (batter (bitter)))
          butter))

(define (depth-max* l)
  (cond ((null? l) 1)
        ((atom? (car l))
         (depth-max* (cdr l)))
        (else
         (max (add1 (depth-max* (car l)))
              (depth-max* (cdr l))))))

(define (leftmost-cc lat)
  (letrec ((fn (lambda (lat skip) 
               (cond ((null? lat) '())
                     ((atom? (car lat)) (skip (car lat)))
                     (else
                      (let ()
                        (fn (car lat) skip)
                        (fn (cdr lat) skip)))))))
   (call/cc (lambda (skip)
     (fn lat skip)))))

(leftmost-cc '(((a)) b (c)) )

(define (leftmost-cc-pretty lat)
  (call/cc
   (lambda (skip)
     (letrec ((fn (lambda (lat) 
                    (cond ((null? lat) '())
                          ((atom? (car lat)) (skip (car lat)))
                          (else
                           (let ()
                             (fn (car lat))
                             (fn (cdr lat))))))))
       (fn lat)))))

(leftmost-cc-pretty '(((z)) b (c)) )

(define (rember1*-new atom l)
  (letrec ((R (lambda (l skip)
                (cond ((null? l) (skip 'no))
                      ((atom? (car l))
                       (cond ((eq? atom (car l))
                              (cdr l))
                             (else (cons (car l) (R (cdr l) skip)))))
                      (else
                       (let ((R-car (call/cc (lambda (skip) (R (car l) skip)))))
                         (if (atom? R-car)
                             (cons (car l) (R (cdr l) skip))
                             (cons R-car (cdr l)))))))))
    (call/cc (lambda (success)
               (call/cc (lambda (fail)
                          (success (R l fail))))
               l))))

(rember1*-new 'noodles '((food) more (food)))
(rember1*-new 'noodles '((food) more (noodles)))

(define x '())
(define food 'stuff)

(define (dinerR food)
  (set! x food)
  (cons 'milkshake
        (cons food
              '())))


(dinerR 'onion)
(displayln x)

(define (chez-nous)
  (let ((temp food))
    (set! food x)
    (set! x temp)))


(displayln food)
(displayln x)

(chez-nous)

(displayln food)
(displayln x)

(define ingredients '())

(define (sweet-toothL food)
  (set! ingredients (cons food ingredients))
  (cons food
        (cons 'cake '())))

(sweet-toothL 'chocolate)
(sweet-toothL 'peacan)
(sweet-toothL 'pie)

(displayln ingredients)

(define (deep n)
  (cond ((zero? n) 'pizza)
        (else (cons (deep (sub1 n)) '()))))

(displayln (deep 0))
(displayln (deep 3))

(define Ns '())
(define Rs '())

(define (deepR n)
  (let ((deep (deep n)))
    (set! Ns (cons n Ns))
    (set! Rs (cons deep Rs))
    deep))

(displayln (deepR 3))
(displayln (deepR 7))
(displayln Ns)
(displayln Rs)
     


;; how to design programs

(define WHEEL_RADIUS 5)
(define WHEEL (circle WHEEL_RADIUS "solid" "black"))
(define WHEELS (overlay/offset WHEEL (* 10 WHEEL_RADIUS) 0 WHEEL))
(define BODY
  (rectangle (+ (* 2 WHEEL_RADIUS) (image-width WHEELS)) (image-height WHEELS) "solid" "red"))
(define WHEELS_BODY
  (overlay/offset WHEELS 0 (/ (* -1 (image-height WHEELS)) 2) BODY))
(define TOP
    (rectangle (/ (image-width BODY) 2) (image-height BODY) "solid" "red"))
(define CAR (overlay/offset WHEELS_BODY 0 (* -1 (image-height BODY)) TOP))
(define CAR_HALF_WIDTH (/ (image-width CAR) 2))

(define TREE_HEAD (circle (* 4 WHEEL_RADIUS) "solid" "green"))
(define TREE_BODY (rectangle (/ (image-width TREE_HEAD) 4) (image-height TREE_HEAD) "solid" "brown"))
(define TREE (overlay/offset TREE_HEAD 0 (image-height TREE_HEAD) TREE_BODY))


(define BACKGROUND (empty-scene 500 100))
(define (move-car x) (place-image CAR x (/ (image-height BACKGROUND) 2) BACKGROUND))
(define (move-car-with-tree x) (place-image TREE
                                            (/ (image-width BACKGROUND) 2)
                                            (- (image-height BACKGROUND) (/ (image-height TREE) 2))
                                            (move-car x)))

(define (scene-position pair) pair)
(define (new-scene-pos percent) (* (sin (* percent 1.570796504)) (* (image-width BACKGROUND) 0.99)))
(define (add-to-scene-position pair)
  (new-scene-pos (/ (scene-position pair) (image-width BACKGROUND))))

;(big-bang 0.1
 ;           [to-draw (lambda (world) (move-car-with-tree (scene-position world))) ]
  ;          [on-tick (lambda (world) (add-to-scene-position world))]
   ;         [on-mouse (lambda (world x y type)
    ;                    (cond ((eq? "button-down" type) x) (else world)))]
     ;       [stop-when (lambda (x) (> (scene-position x) (image-width BACKGROUND) ))]
      ;      )

;; OTHER

(define (things f)
   (letrec-values
       (((listener) (tcp-listen 8000 5 #t))
        ((in out) (tcp-accept listener)))
     (f in out listener)))

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
