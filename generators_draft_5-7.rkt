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


