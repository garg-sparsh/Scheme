;Sparsh Garg
;Assignment-1 Scheming
;W1377715

;1 Takes a list comprised of numerical values and returns the largest one.
(define  (find-biggest a-list)
 (if (null? a-list) (begin (display "Empty list")(newline)) ;checking if list is empty
  (let largest ((a-list (cdr a-list)) (maxv (car a-list))) ; assign first value of list to maxv
   (cond
    ((null? a-list) maxv) ;display the max number
    ((> (car a-list) maxv) (largest (cdr a-list) (car a-list))) ;if next value is greater than current max then swap the value
    (#t (largest (cdr a-list) maxv)))))) ;recursion to iterate the list 


;2 Takes two integer parameters: a starting and an ending value. Prints all integers between those two number (inclusive).
(define (count-from x y) 
 (cond ((= x y) ;checking if x = y if true then display x
  (begin (display x) (newline))) 
   (#t (begin (display x)
     (newline) 
       (count-from (+ x 1) y))))) ;iterate by recursion after incrementing 1 


;3 Takes two parameters: an index (position) and a list. Returns the item in the list corresponding to the index specified.  
(define (lengthL a-list) ;length of the list
 (cond ((null? a-list) 0) 
  (#t (+ 1 (lengthL(cdr a-list))))))

(define (nth-item x a-list)
 (cond  ((> x (lengthL a-list)) ;if given index is greater than length then display invalid
  (display "Invalid Index"))
   ((>= 0 x)
    (display "index > 0")) ;if index is less than or equal to 0
     (#t (cond ((= x 1)
      (car a-list))
       (#t (nth-item (- x 1) ;iterating by decrementing the x and car the list by decrementing the a-list using cdr
        (cdr a-list)))))))


;4 Takes three parameters: an index (position), a list, and a value to substitute into the list. Returns the list, with the item at the specified index replaced with the new value.
(define (replace-nth-item x a-list val) 
 (cond ((> x (lengthL a-list)) ;if given index is greater than length then display invalid
   (display "Invalid Index")) 
    ((>= 0 x)(display "index > 0"); if index is less than or equal to 0
     (newline)) 
      (#t (cond ((= x 1) 
       (cons val (cdr a-list))) ;adding the val to the top of the list
        (#t (cons (car a-list)
         (replace-nth-item (- x 1) ;iterating by decrementing the x and decrementing the a-list using cdr
          (cdr a-list) val)))))))



;5 Takes a single parameter: a list. Returns a boolean value indicating if that list is in a (numerically) sorted order. 

(define  (sorted-asc? a-list) ;check if list is sorted in inc order
 (if (null? a-list) (begin (display "Empty list")(newline)) ;if null is empty
  (let largest ((a-list (cdr a-list)) (maxv (car a-list))) ;assign the max value to first value of list
   (cond
    ((null? a-list) (let ((x '#t)) x))
    ((< (car a-list) maxv) (let ((x '#f)) x));comparing it with the next value
    (#t (largest (cdr a-list) (car a-list))))))) ;assign the next value to max value and iterate it for comparison

(define  (sorted-desc? a-list) ; check if list is sorted in dec order 
 (if (null? a-list) (begin (display "Empty list")(newline)) ;if null is empty
  (let largest ((a-list (cdr a-list)) (maxv (car a-list))) ;assign the max value to first value of list
   (cond
    ((null? a-list) (let ((x '#t)) x))
    ((> (car a-list) maxv) (let ((x '#f)) x)) ;comparing it with the next value
    (#t (largest (cdr a-list) (car a-list)))))));;assign the next value to max value and iterate it for comparison

(define (sorted? a-list) ;displaying whether the list is sorted or not
 (cond
 ((equal? (sorted-asc? a-list) '#t) #t)
 ((equal? (sorted-desc? a-list) '#t) #t)
 (#t #f)))

;6 Takes two parameters: an action and a state. Returns the updated state that will result by following that action. A state is a list of three elements: an X position, a Y position, and a direction (N, S, E, or W). An action is one of the following strings: STAY, MOVE-1, MOVE-2, MOVE-3, TURN-LEFT, TURN-RIGHT, or TURN-AROUND. Assume that all moves are forward, relative to the current direction.


(define (apply-action a-list action) 
 (cond ((string=? action "STAY") a-list)
  ((string=? action "MOVE-2")  ;action is move-2
   (cond
     ((equal? (nth-item 3 a-list) 'N) (replace-nth-item 2 a-list (+ (nth-item 2 a-list) 2))) ;checking if the position is north and replace the y axis by y+2
     ((equal? (nth-item 3 a-list) 'S) (replace-nth-item 2 a-list (- (nth-item 2 a-list) 2))) ;checking if the position is south and replace the y axis by y-2
     ((equal? (nth-item 3 a-list) 'W) (replace-nth-item 1 a-list (- (nth-item 1 a-list) 2))) ;checking if the position is West and  replace the x axis by x+2 
     ((equal? (nth-item 3 a-list) 'E) (replace-nth-item 1 a-list (+ (nth-item 1 a-list) 2))))); checking if the position is East and replace the x axis by x-2
  ((string=? action "MOVE-1")
   (cond
     ((equal? (nth-item 3 a-list) 'N)  (replace-nth-item 2 a-list (+ (nth-item 2 a-list) 1)));checking if the position is north and replace the y axis by y+1
     ((equal? (nth-item 3 a-list) 'S)  (replace-nth-item 2 a-list (- (nth-item 2 a-list) 1)));checking if the position is south and replace the y axis by y-1
     ((equal? (nth-item 3 a-list) 'W)  (replace-nth-item 1 a-list (- (nth-item 1 a-list) 1)));checking if the position is West and  replace the x axis by x+1
     ((equal? (nth-item 3 a-list) 'E)  (replace-nth-item 1 a-list (+ (nth-item 1 a-list) 1)))));checking if the position is east and  replace the x axis by x-1
  ((string=? action "MOVE-3")
   (cond
     ((equal? (nth-item 3 a-list) 'N)  (replace-nth-item 2 a-list (+ (nth-item 2 a-list) 3)));checking if the position is north and replace the y axis by y+3
     ((equal? (nth-item 3 a-list) 'S)  (replace-nth-item 2 a-list (- (nth-item 2 a-list) 3)));checking if the position is south and replace the y axis by y-3
     ((equal? (nth-item 3 a-list) 'W)  (replace-nth-item 1 a-list (- (nth-item 1 a-list) 3)));checking if the position is West and  replace the x axis by x+3
     ((equal? (nth-item 3 a-list) 'E)  (replace-nth-item 1 a-list (+ (nth-item 1 a-list) 3)))));checking if the position is East and replace the x axis by x-3
  ((string=? action "TURN-AROUND")
   (cond
     ((equal? (nth-item 3 a-list) 'N)  (replace-nth-item 3 a-list 'S)) ;checking if the position is north and replace the position with south
     ((equal? (nth-item 3 a-list) 'S)  (replace-nth-item 3 a-list 'N));checking if the position is south and replace the position with north
     ((equal? (nth-item 3 a-list) 'W)  (replace-nth-item 3 a-list 'E));checking if the position is West and replace the position with east
     ((equal? (nth-item 3 a-list) 'E)  (replace-nth-item 3 a-list 'W))));checking if the position is east and replace the position with west
  ((string=? action "TURN-RIGHT") ;now replcaing with the right direction
   (cond
     ((equal? (nth-item 3 a-list) 'N)  (replace-nth-item 3 a-list 'E))
     ((equal? (nth-item 3 a-list) 'S)  (replace-nth-item 3 a-list 'W))
     ((equal? (nth-item 3 a-list) 'W)  (replace-nth-item 3 a-list 'N))
     ((equal? (nth-item 3 a-list) 'E)  (replace-nth-item 3 a-list 'S))))
  ((string=? action "TURN-LEFT");now replcaing with the  left direction
   (cond
     ((equal? (nth-item 3 a-list) 'N)  (replace-nth-item 3 a-list 'W))
     ((equal? (nth-item 3 a-list) 'S)  (replace-nth-item 3 a-list 'E))
     ((equal? (nth-item 3 a-list) 'W)  (replace-nth-item 3 a-list 'S))
     ((equal? (nth-item 3 a-list) 'E)  (replace-nth-item 3 a-list 'N))))
))



; Advanced Assignment

(define percept   ;initialise the percept 
'((empty empty empty)  
  (empty (vegetation 2 45) empty empty empty)  
  ((vegetation 3 150) empty empty empty empty empty barrier)  
  (barrier empty empty empty empty empty empty barrier barrier)  
  (barrier barrier empty (vegetation 4 200) empty empty empty   (vegetation 1 125) barrier barrier barrier))
)

(define (get-location a-list x y) ;getting the location of y axis first and then x-axis
  (let ((y_cord (nth-item y a-list)))
   (nth-item (+ x y 1) y_cord)))


; Extra Credits
;
;Takes two lists of integers, each assumed to be in ascending order. Returns a single list comprised of the values from both lists in ascending order.

(define (merge-ordered-lists a-list b-list)
 (cond
  ((and (null? a-list) (null? b-list)) '())
  ((null? a-list) b-list)
  ((null? b-list) a-list)
  (#t (if (< (car a-list) (car b-list)) (cons (car a-list) (merge-ordered-lists (cdr a-list) b-list)) (cons (car b-list) (merge-ordered-lists (cdr b-list) a-list))))
))



;Takes one parameter, a list of integer values. Returns a list with the same values, sorted in increasing numerical order. Merge sort will operate by splitting the list in half (+/- 1 element), sorting each of those lists (recursively, using merge-sort), and then merging the two sorted lists

(define (check-number x)
 (cond
 ((= x 0) #t)
 ((= x 1) #f)
 (#t (check-number (- x 2)))))

(define (first x a-list)
 (cond
  ((= x 0) '())
  ((append (first (- x 1) a-list) (list(nth-item x a-list)) ))
))

(define (firstL a-list)
 (cond
 ((check-number (lengthL a-list)) (first (/ (lengthL a-list) 2) a-list))
 (#t (first (/ (- (lengthL a-list) 1) 2) a-list))))


(define (second x y a-list) ;second half of the list
 (cond
 ((> x y) '())
 ((append (list(nth-item x a-list)) (second (+ x 1) y a-list)))
))

(define (secondL a-list)
 (cond
  ((check-number (length a-list)) (second (+ (/ (lengthL a-list) 2) 1) (lengthL a-list) a-list))
  (#t (second (/ (+ (lengthL a-list) 1) 2)  (lengthL a-list) a-list))
))

(define (break a-list) ; split the list into two
 (cons (firstL a-list) (cons (secondL a-list) '())))

(define (merge-sort a-list) ;merging the sorted list
 (cond
  ((null? a-list) a-list)
  ((null? (cdr a-list)) a-list)
  (#t (merge-ordered-lists (merge-sort (car (break a-list))) (merge-sort (cadr (break a-list)))))))



