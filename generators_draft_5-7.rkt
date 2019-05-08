;;Anthony Tsui

;;Generators Project using continuations for CS 335 Programming Language Paradigms
;;Professor Douglas Troeger


;;Initial project idea is to built a generator for use with trees
;;Tree will be represented as lists in inorder traversal, and in the following form:
;; assume the binary tree has the form:
;              5
;            /   \
;           3     8
;          /     / \
;         2     6   9
;                \
;                 7
;then what we pass into our tree generator is '(((2)3)5((6(7))8(9)))
;Our tree-generator, or more aptly: leaf generator would then return back the values
;of the leaves it encounters one at a time.
;In this example if we pass '(((2)3)5((6(7))8(9))) to our generator we should get in return:
;2 -> 3 -> 5 -> 6 -> 7 -> 8 -> 9
;
;Since this is similar to a tree traversal we can use that as a approach to base the generator on

(define traverseTree
  (lambda(tree)
    (cond
      ((null? tree) 'End)    ;;Finished going through the tree of the tree is empty
      ((pair? tree) (traverseTree (car tree)) (traverseTree (cdr tree)))  ;;Inorder traversal so (car tree) first
      (else                  ;;Not empty, and not pair so it must be a leaf
       (begin                ;;Just using begin so I can add newline as well as display
         (display tree)      ;;Display leaf 
         ;(newline)
         )))))

(traverseTree '())

(define testTree '((1 2) 3))
(traverseTree testTree)
;Outputs 123end

(define testTree2 '(((2)3)5((6(7))8(9))))
(traverseTree testTree2)
;Outputs 2356789end


;;Our generator needs to have at least one continuation to keep track of where it is
;(define generator
;        (lambda (tree)
;          (cond 
;             (if null: return end)
;             (if pair: call the functions again on car and cdr
;                       Since this is inside the generator we'll want to define a function here?)
;             (if leaf: we return leaf along with the current continuation of where we are in the traversal
;                       something akin to (call

(define call/cc call-with-current-continuation) ;Shortening the name so it's actually bearable



;;Testing to see if we can save the continuations for later use to a variable instead of passing it
(define testLambdaNoArgs
  (lambda () (display 'This.Works)))

(testLambdaNoArgs)
(newline)


(+ 2 (call/cc (lambda (currCont)
         (+ 5 (currCont 3)))))   ;;Escapes the 5 and goes straight to (+ 2 3)

(define testCurr #f)

(+ 2 (call/cc (lambda (currCont)
         (set! testCurr currCont)
         (+ 5 (currCont 3)))))

(testCurr 10) ;This outputs twelve, meaning we immediatly escape with 10 as our value and go to (+ 2 10)


(define loopCont #t)

(define thruTree
  (lambda(list)
    (cond
      ((null? list) 'End)
      ((pair? list) (thruTree (car list)) (thruTree (cdr list)))
      (else
       (begin
         (call/cc (lambda (thruCont)
                    (set! loopCont thruCont)    ;;This should save the continuation, now we need to switch continuations
                    ;It should look something like (switchContinuation list) where list is the value we want to return
         (display list)
         (newLine))))))))

;(thruList '(1 2 3 4 5))

;New Approach: Maybe store the state of the generator inside of it ?
;For instance when we call generator, it immediately stores the continuation of where it was called
;in order to escape the function once we find a leaf. However we also need a continuation to store where we are inside
;the tree. So we need two continuations (?)


'Testing.Adding.Continuations
(newline)

(define tree '((1 2) 3))

(define savedCont #t)

(define testFunction
  (lambda ()
    (let iter ((toParse tree))     ;;Looks like I need to assign tree to a different variable first if I want to use let
      (cond
        ((null? toParse) 'end)
        ((pair? toParse) (iter (car toParse)) (iter (cdr toParse)))
        (else
         (call/cc (lambda(currCont)
                    (set! savedCont currCont) ;Actually can we just save the iter function to this here??
                    (display toParse) ;Remove once we found out how to parse
                    (newline)         ;need to remove
                    ;Need to escape here with toParse, which is the leaf value
           
           )))))))

(testFunction)

;Brainstorming skeleton for the generator:
;As soon as it is called we need to save the continuation to escape back with our leaf value
;Second continuation we need is to keep track of where we are in the tree
;We will need to keep both states/continuations inside the generator (?)

;Skeleton:
;(define generator
;   (let ((escapeContinuation #t))  ;initialize escapeContinuation and set it to the curr continuation immediately?
;      (let iter ((toParse tree))   ;basically what we have above
;           (cond
;               (null? tree  ...
;               (pair? tree  ... iter(car and cdr tree)
;               (else                ;found leaf so now we save our place and then use escapeContinuation?
;                    (call/cc (lambda (currCont)
;                             (set! iter currCont) If this lets us go back to where we are currently in iter
;                             (escape with (escapeContinuation tree) which should return back to the original call and
;                                                                    bring the leaf value with us?
;           ;We need to set an escape condition for after we finish. Maybe just escape with #f or '() 
;           ;Also store the escapeContinuation here? not sure which let to leave it inside of



;
;Finish making the generator and start making comparison functionalities. (this will probably be trivial in comparison
;                                                                           if we just end up using the tree gen)