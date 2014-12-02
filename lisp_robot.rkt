;Marcus Tedesco
;#905373051

;definition of the input tree
(define tree
'(73
   ( 49
        ( 15
             ( 10 () () )
             ( 20
                  ( 17 () () )
                  ( 30
                       ()
                       ( 42 () () )
                  )
             )
        )
        ( 53 ()
             ( 64 () () )
        )
   )

   ( 134
         ( 133
               ( 94
                    ( 82
                         ( 75 () () )
                         ()
                    )
                    ( 108
                          ( 103 () () )
                          ( 110 () () )
                    )
               )
               ()
         )
         ( 135
               ()
               ( 136
                     ()
                     ( 152
                           ( 141 () () )
                           ()
                     )
               )
         )
   )
)
)

;checks if the goal is in the tree
(define (isInTree goal subtree)
  (cond ((null? subtree)   ;if the tree is empty
         #f)               ;return false
        ((= (car subtree) goal)    ;if the current element is goal
         #t)                       ;return true
        ((< goal (car subtree))               ;if goal is less than current element
         (isInTree goal (cadr subtree)))      ;check left subtree
        (else                                 ;if goal is more than current element
         (isInTree goal (caddr subtree))))    ;check right subtree
) 

;returns the path to the goal
(define (robot goal subtree)
  (cond ((= (car subtree) goal)    ;if the current element is goal
         (cons (car subtree) '()))      
        ((< goal (car subtree))    ;if goal is less than current element
         (cons (car subtree) (cons 'R (robot goal (cadr subtree)))))      
        (else                      ;if goal is more than current element
         (cons (car subtree) (cons 'L (robot goal (caddr subtree))))))
)

  ;this could be done without 2 different functions
  ;a boolean value could be appended to the end of the list
  ;if the list ended in #t it would print the list
  ;if #f it would print not found: #
  ;I choose this way because it was more readable and 
  ;although inefficient because it traverses twice,
  ;it does not change the time complexity of the program
  ;also the spec does not say that booleans cannot be used
(define (navigate goal tree)
  (cond ((isInTree goal tree)
         (cons 'found: (robot goal tree)))
        (else
         (cons 'not (cons 'found: (cons goal '())))))   
)

(navigate 42 tree)
(navigate 141 tree)
(navigate 103 tree)
(navigate 81 tree)
(navigate 73 tree)