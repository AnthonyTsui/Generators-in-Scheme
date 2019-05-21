(define call/cc call-with-current-continuation)

;List Generator

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

(display 'Start_of_listLoop_Example)
(newline)

(listLoop list1)

(display 'End_of_listLoop_Example)
(newline)


;Tree generator (iterates through nodes)

(define treeGen
  (lambda (tree)
    (let ((escapeCont 'Initial))
      (letrec ((generate-node
                (lambda ()
                  (let loop ((tree tree))
                    (cond ((null? tree) 'Skip)
                          ((pair? tree) (loop (car tree))
                                        (loop (cdr tree)))
                          (else (call/cc
                                 (lambda(currPos)
                                   (set! generate-node currPos)
                                   (escapeCont tree))))))
                  (escapeCont '()))))
        (lambda()
          (call/cc
           (lambda (toEscape)
             (set! escapeCont toEscape)
             (generate-node))))))))

(define treeLoop
  (lambda (tree)
    (let ((treeGener (treeGen tree)))
      (let loop()
        (let ((node (treeGener)))
          (cond ((null? node) '())
                (else (begin
                        (display node)
                        (newline)
                        (loop)))))))))

(define tree1 '(((2)3)5((6(7))8(9))))

(define tree2 '(((2)3)5((6(11))8(9))))

(display 'Start_of_treeLoop_example)
(newline)

(treeLoop tree1)

(display 'End_of_treeLoop_example)
(newline)


;Using two generators to compare two trees

(define treeCompare
  (lambda (tree1 tree2)
    (let ((treeGen1 (treeGen tree1))
          (treeGen2 (treeGen tree2)))
      (let loop()
        (let ((node1 (treeGen1))
              (node2 (treeGen2)))
          (cond ((eq? node1 node2)
                 (cond ((null? node1) #t)
                       (else (loop))))
                (else #f)))))))

;(treeCompare tree1 tree1)
;(treeCompare tree1 tree2)

(define findElem
  (lambda (tree1 elem)
    (let ((treeGener (treeGen tree1)))
      (let loop()
        (let ((node (treeGener)))
          (cond ((null? node) #f)
                (else (cond ((eq? node elem) #t)
                            (else (loop))))))))))

;(findElem tree1 6)
;(findElem tree1 1)



(define-syntax for
  (syntax-rules (in)
    ((_ node in (tree) body ...)
     (let ((treeGener (treeGen tree)))
       (let loop()
         (let ((node (treeGener)))
           (cond ((null? node) '())
                 (else (begin
                         body ...
                         (loop))))))))))
     




(for node in (tree1)
  (display node)
  (newline))





