(define call/cc call-with-current-continuation)

(define listGen
  (lambda (list)
    (let ((escapeCont '()))
      (letrec ((generate-list
                (lambda ()
                  (let loop ((list list))
                    (cond ((null? list) 'Skip)
                          (else (begin
                                   (call/cc
                                    (lambda(currPos)
                                      (set! generate-list currPos)
                                      (escapeCont (car list))))
                                   (loop (cdr list))))))
                  (escapeCont '()))))
        (lambda()
          (call/cc
           (lambda(toEscape)
             (set! escapeCont toEscape)
             (generate-list))))))))

(define listLoop
  (lambda (list)
    (let ((listGenerator (listgen list)))
      (let loop()
        (let ((leaf (listGenerator)))
          (cond ((null? leaf) '())
                (else (begin
                        (display leaf)
                        (loop)))))))))

(define list1 '(1 2 3 4 5 6 7))

(listLoop list1)

(define treeGen
  (lambda (tree)
    (let ((escapeCont 'Initial))
      (letrec ((generate-leaf
                (lambda ()
                  (let loop ((tree tree))
                    (cond ((null? tree) 'Skip)
                          ((pair? tree) (loop (car tree))
                                        (loop (cdr tree)))
                          (else (call/cc
                                 (lambda(currPos)
                                   (set! generate-leaf currPos)
                                   (escapeCont tree))))))
                  (escapeCont '()))))
        (lambda()
          (call/cc
           (lambda (toEscape)
             (set! escapeCont toEscape)
             (generate-leaf))))))))

(define treeLoop
  (lambda (tree)
    (let ((treeGener (treeGen tree)))
      (let loop()
        (let ((leaf (treeGener)))
          (cond ((null? leaf) '())
                (else (begin
                        (display leaf)
                        (newline)
                        (loop)))))))))

(define tree1 '(((2)3)5((6(7))8(9))))

(define tree2 '(((2)3)5((6(11))8(9))))

(treeLoop tree1)

(define treeCompare
  (lambda (tree1 tree2)
    (let ((treeGen1 (treeGen tree1))
          (treeGen2 (treeGen tree2)))
      (let loop()
        (let ((leaf1 (treeGen1))
              (leaf2 (treeGen2)))
          (cond ((eq? leaf1 leaf2)
                 (cond ((null? leaf1) #t)
                       (else (loop))))
                (else #f)))))))

(treeCompare tree1 tree1)
(treeCompare tree1 tree2)

(define findElem
  (lambda (tree1 elem)
    (let ((treeGener (treeGen tree1)))
      (let loop()
        (let ((leaf (treeGener)))
          (cond ((null? leaf) #f)
                (else (cond ((eq? leaf elem) #t)
                            (else (loop))))))))))

(findElem tree1 6)
(findElem tree1 1)


;(define-syntax for
;  (syntax-rules (in)
;    ((_ leaf in (tree) body ...)
;     (let loop()
;       (let ((leaf (tree)))
;         (cond ((null? leaf) '())
;               (else (begin
;                       body ...
;                       (loop)))))))))

(define-syntax for
  (syntax-rules (in)
    ((_ leaf in (tree) body ...)
     (let ((treeGener (treeGen tree)))
       (let loop()
         (let ((leaf (treeGener)))
           (cond ((null? leaf) '())
                 (else (begin
                         body ...
                         (loop))))))))))
     


(define testGen3 (treeGen tree1))

(for leaf in (tree1)
  (display leaf)
  (newline))
    