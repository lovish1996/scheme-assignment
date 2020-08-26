#lang racket
(provide (all-defined-out))

;;(define inputFile (vector-ref (current-command-line-arguments) 0))
;;;;; Step1 Begins ;;;;;

;; Read from the input file
(define inpPort_1 (open-input-file "t0.in"))
(define outPort_1 (open-output-file "xyz.out"
                                 #:exists 'append))
(define N (read inpPort_1))
(define D (read inpPort_1))
(define K (read inpPort_1))
(define ε (read inpPort_1))
(define MinPts (read inpPort_1))

(define dim D)

;; Reading a datapoint from input file and constructing a list of a given data point
(define (dataPointList dim)
  (if (eqv? dim 0)
     '()
     (cons (read inpPort_1) (dataPointList (- dim 1)))
     )
  )

;; Reading all the datapoints from dataset D and constructing a list out of it
(define (listDataPoints cnt dim)
  (if (eqv? cnt (+ N 1))
      '()
      (cons (list cnt (dataPointList dim)) (listDataPoints (+ cnt 1) dim))
      )
  )

;; counter variable to control the no. of data points in given dataset
(define cnt 1)
(define step1 (listDataPoints cnt dim))

;;(display step1)   ;;to display result
;;(displayln step1)

;; Writing result of Step1 to Output port
;;(display step1 outPort_1)
(displayln step1 outPort_1)


;;closing Input port after reading Input
(close-input-port inpPort_1)

;;;;; Step1 Over ;;;;;

;;;;; Step2 Begins ;;;;;

;; Precision setting
(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

;; utility function to decide whether two data points are identical
(define (similar? l1 l2)
 (cond
   ((null? l1) (null? l2))
   ((null? l2) #f)
   ((equal? (car l1) (car l2)) (similar? (cdr l1) (cdr l2)))
   (else #f)
   )
 )

;;(displayln (move_to_list step1 35))
;;(displayln (move_to_list step1 38))

;; utility function for calculating Euclidean distance
(define (euclidean l1 l2)
  (if
   (null? l1) 0
    (+ (expt (- (car l1) (car l2)) 2) (euclidean (cdr l1) (cdr l2)))
    )
)

;; Functions to calculate euclidean distance
(define (distance l1 l2)
  (if
   (similar? l1 l2) +inf.0
  (sqrt (euclidean (cadr l1) (cadr l2) )
  )
)
  )

;; result of step1 stored as list
(define temp_list step1)  

;; utility function to generate list of euclid distance for each data point
(define (sparMatr_util l1 k cnt l2)
 (if
  (equal? k 0) '()
  (cons (list cnt (distance l1 (car l2))) (sparMatr_util l1 (- k 1) (+ cnt 1) (cdr l2)))
  )
  )

;; generating similarity matrix for all the data points
(define (sparMatr l1 k)
  (if
   (equal? k 0) '()
   (cons (sparMatr_util (car l1) N 1 temp_list) (sparMatr (cdr l1) (- k 1)))
)
  )

;; result of Step2 stored as a list
(define tstep2 (sparMatr temp_list N))
(define step2 (modify_precision tstep2))


;;(displayln step2)
;;Writing result of Step2 to Output port
(displayln step2 outPort_1)

;;;;; Step2 Over ;;;;;

;;;;; Step3 Begins ;;;;;

;; storing result of step2 in a temporary list
(define temp_list_2 step2)

;; customized sort function to sort lists by second element and then by least first element
(define (sortByEuclidDistance l1)
  (sort l1 (lambda (x y) (cond
                           ((< (- (cadr x) (cadr y)) 0)(< (- (cadr x) (cadr y)) 0))
                           ((> (- (cadr x) (cadr y)) 0)(< (- (cadr x) (cadr y)) 0))
                           (else (< (- (car x) (car y)) 0))
                            )

             ))
  )

;;(sortByEuclidDistance (sparMatr_util (cadar temp_list) N 1 temp_list))
;;(sortByEuclidDistance '((1 2) (4 3) (2 4) (3 6) (2 3) ))

;; utility function to filter k Nearest neighbour points (lists including index of point and its euclidean distance)
(define (kNN_util_3 l1 k)
 (if
     (equal? k 0) '()
     (cons (caar l1) (kNN_util_3 (cdr l1) (- k 1)))
     )
  )

;; utility function for sorting list of k Nearest neighbour by its index
(define (kNN_util_2 l1 k)
  (sort (kNN_util_3 l1 k) <)
  )

;; utility function for sorting input row of similarity matrix by Euclid distance 
(define (kNN_util l1 k)
  (kNN_util_2 (sortByEuclidDistance l1) k)
  )

;; main function for finding k Nearest Neighbour of given data point and constructing list for all data points in dataset
(define (kNN l1 k)
  (if
     (equal? k 0) '()
      (cons (kNN_util (car l1) K) (kNN (cdr l1) (- k 1)))      
            ) 
  )

;; storing result of step3 in a list for further calculations
(define step3 (kNN step2 N))

;;(displayln step3)

;;Writing result of Step3 to Output port
(displayln step3 outPort_1)

;;;;; Step3 Over ;;;;;

;;;;; Step4 Begins ;;;;;

;; member function
(define (member a l1)
(cond
  ((null? l1) #f)
  ((equal? a (car l1)) #t)
  (else (member a (cdr l1)))
  )
  )

;; function to calculate no. of common elements in two lists
(define acc 0)
(define (common_elements l1 l2 acc)
  (cond
    ((null? l1) acc)
    ((member (car l1) l2) (common_elements (cdr l1) l2 (+ acc 1)))
    (else (common_elements (cdr l1) l2 acc))
   )
  )

;;(displayln (common_elements '(7 6 5 4 2 3 1) '(1 2 3 4 5 6 7) acc))
;;(displayln (common_elements '(45 82 5 2 4 6 7) '(1 2 3 4 5 6 7) acc))

;; length of list
(define (length l1 acc)
  (cond
    ((null? l1) acc)
    (else (length (cdr l1) (+ acc 1)))
   )
  )
;; function to move to a given sublist in list of sublists
(define (move_to_list l1 k)
  (cond
   ((> k (length l1 acc)) '())
   ((equal? k 1) (car l1))
   (else (move_to_list (cdr l1) (- k 1)))
   )
  )

;; function to move to a given sublist in list of sublists
(define (move_to_list_m l1 k)
  (cond
   ((equal? k 1) (car l1))
   (else (move_to_list_m (cdr l1) (- k 1)))
   )
  )
;;(displayln (move_to_list step3 10))

;; function to check whether list has element b and list b has element a i.e, will there be edge between a and b in shared neighbour graph
(define (in_graph? l1 a b)
  (cond
    ((member a (move_to_list l1 b)) (member b (move_to_list l1 a)))
    (else #f)
    )
  )

;;(in_graph? step3 12 2)
;;(member 8 (move_to_list step3 5))
;;(member 1 (move_to_list step3 1))

;; customized sort function to sort lists giving higher preference to edges with higher weight and then by least index
(define (sortByWeights l1)
  (sort l1 (lambda (x y) (cond
                           ((< (- (cadr x) (cadr y)) 0)(> (- (cadr x) (cadr y)) 0))
                           ((> (- (cadr x) (cadr y)) 0)(> (- (cadr x) (cadr y)) 0))
                           (else (< (- (car x) (car y)) 0))
                            )

             ))
  )

;;(sortByWeights '((13 3) (12 4) (14 3) (5 3) (8 4)))

;; utility function to construct shared neighbour graph for a particular datapoint in dataSet
(define (graph_util_2 l1 trap)
  (cond
    ((null? l1) '())
    ((in_graph? step3 (car l1) trap)(cons (list (car l1) (common_elements (move_to_list step3 trap) (move_to_list step3 (car l1)) acc)) (graph_util_2 (cdr l1) trap)))
    (else (graph_util_2 (cdr l1) trap))
    )
  )

;; utility function to construct graph for individual datapoint
(define (graph_util l1 trap)
  (sortByWeights (graph_util_2 l1 trap))
  )
;; main function to construct shared neighbour graph G
(define (graph l1 k)
  (if
   (equal? k 0) '()
   (cons (graph_util (car l1) (- N (- k 1))) (graph (cdr l1) (- k 1))))
   )
  
;; storing result of step4 in a list for further calculations
(define step4 (graph step3 N))

;;(displayln step4)

;;Writing result of Step4 to Output port
(displayln step4 outPort_1)

;;;;; Step4 Over ;;;;;

;;;;; Step5 Begins ;;;;;

;; utility function to count point density for a given data point in dataset
(define (point_density_util l1 acc)
  (cond
    ((null? l1) acc)
    ((>= (cadar l1) ε)(point_density_util (cdr l1) (+ acc 1)))
    (else (point_density_util (cdr l1) acc))
    )
    )
  
;; main function to calculate point_density list of overall points available in data set
(define (point_density l1 k)
  (cond
    ((null? l1) '())
    (else (cons (point_density_util (car l1) acc) (point_density (cdr l1) (- k 1)))))
    )
  
;; storing result of step5 in a list for further calculations
(define step5 (point_density step4 N))

;;(displayln step5)

;;Writing result of Step5 to Output port
(displayln step5 outPort_1)

;;;;; Step5 Over ;;;;;

;;;;; Step6 Begins ;;;;;

;; variable to store initial value of index in core_point list
(define acc1 (+ acc 1))

;; Function to generate list of core points
(define (core_point l1 k)
  (cond
     ((null? l1) '())
     ((>= (car l1) MinPts) (cons k (core_point (cdr l1) (+ k 1))))
     (else (core_point (cdr l1) (+ k 1)))
     )
  )

;; storing result of step6 in a list for further calculations
(define step6 (core_point step5 acc1))

;;(displayln "Core Points")
;;(displayln step6)

;;Writing result of Step6 to Output port
(displayln step6 outPort_1)

;;;;; Step6 Over ;;;;;

;;;;; Step7 Begins ;;;;;

;; function to move to a given sublist in list of sublists

;; list to traverse through core points for clustering
(define temp_core_points step6)

;; utility function to calculate difference of two lists
(define (list_difference l1 l2)
  (cond
    ((null? l1) '())
    ((null? l2) l1)
    ((not (member (car l1) l2)) (cons (car l1) (list_difference (cdr l1) l2)))
    (else (list_difference (cdr l1) l2))
    )
  )

;; customized sort function to sort lists by first element 
(define (sortByFirstArgument l1)
  (sort l1 (lambda (x y) (< (- (car x) (car y)) 0)
             ))
  )

;;(sortByFirstArgument (move_to_list step4 5))

#|;; returns first element in a list with first argument > k
(define (clustering_util4 l1 k)
  (cond
  ((null? l1) 0)
  ((and (and (> (car(car l1)) k) (>= (cadar l1) ε)) (member (caar l1) step6)) (caar l1))
  (else (clustering_util4 (cdr l1) k))
  )
  )

;;(clustering_util4 (sortByFirstArgument(move_to_list step4 14)) 14)
;;(sortByFirstArgument(move_to_list step4 14))

;; utility function to construct a particular cluster
(define (clustering_util3 l1 k)
  (cond
    ((or (null? l1) (equal? (clustering_util4 l1 k) 0)) '())
    (else (cons (clustering_util4 l1 k) (clustering_util3 (sortByFirstArgument(move_to_list step4 (clustering_util4 l1 k))) (clustering_util4 l1 k))))
    )
  )

;; passing sorted adjacency list of argument  
(define (clustering_util2 l1 k)
  (cons k (clustering_util3 (sortByFirstArgument (move_to_list step4 k)) k)
  )
 )

;; generating a particular cluster (l1 is remaining list of core points and (car l1) is first argument of remaining list)
(define (clustering_util l1)
  (clustering_util2 l1 (car l1))
  )|#

#|(define (move_to_list l1 k)
  (cond
   ((equal? k 1) (car l1))
   (else (move_to_list (cdr l1) (- k 1)))
   )
  )|#

;;(move_to_list step4 5)

(define (points_x l1 l2)
  (cond
    ((null? l1) '())
    ((and (>= (cadar l1)ε) (member (caar l1) l2)) (cons (caar l1) (points_x (cdr l1) l2)))
    (else (points_x (cdr l1) l2))
   )
)

;;(move_to_list_m step4 3)
;;(points_x (move_to_list_m step4 3) '(1 3 4 5 7 8 12 14 16 20))


(define (clustering_util3 l1 l2)
  (cond
  ((null? l1) '())
  (else (append (cons (car l1)(points_x (move_to_list step4 (car l1)) l2)) (clustering_util3 (cdr l1) l2)))
  )
)



(define (clustering_util2 l1 l2)
  (cond
    ((equal? (sort l1 <) (sort (clustering_util3 l1 l2) <)) l1)
    (else (clustering_util2 (sort (clustering_util3 l1 l2) <) (list_difference l2 (clustering_util3 l1 l2))))
   )
  )

(define (clustering_util l1)
  (clustering_util2 (list (car l1)) (cdr l1))
  )
;; constructing clustering list here
(define(clustering l1 k)
  (cond
    ((null? l1) '())
    (else (cons (list k (remove-duplicates(clustering_util l1))) (clustering (list_difference l1 (clustering_util l1)) (+ k 1))))
    )
  )


;; storing result of step7 in a list for further calculations
(define step7 (clustering temp_core_points acc1))
;;(displayln step3)
;;(displayln step7)

;;Writing result of Step7 to Output port
(displayln step7 outPort_1)

(define (list_for_clusters l1)
  (cond
    ((null? l1) '())
    ((>= (cadar l1) ε) (cons (caar l1) (list_for_clusters (cdr l1))))
    (else (list_for_clusters (cdr l1)))
                       
    )
  )


;;;;; Step7 Over ;;;;;

;;;;; Step8 Begins ;;;;;

;; utility function to check whether a given datapoint satisfies the conditions of a Noise point
(define (noise_points_util l1)
  (cond
    ((null? l1) #t)
    ((< (cadar l1) ε) (noise_points_util (cdr l1)))
    (else #f)
    )
  )

;;function to accumulate all noise points in a list
(define (noise_points l1 k)
  (cond
    ((null? l1) '())
    ((and (not (member k step6)) (noise_points_util (car l1))) (cons k (noise_points (cdr l1) (+ k 1))))
    (else (noise_points (cdr l1) (+ k 1)))
    )
  )

;; storing result of step8 in a list for further calculations
(define step8 (noise_points step4 acc1))

;;(displayln step8)

;;Writing result of Step8 to Output port
(displayln step8 outPort_1)

;;;;; Step8 Over ;;;;;

;;;;; Step9 Begins ;;;;;

;;function to accumulate all border points in a list
(define (border_points k)
  (cond
    ((equal? k (+ N 1)) '())
    ((and (not (member k step6)) (not (member k step8))) (cons k (border_points (+ k 1))))
    (else (border_points (+ k 1)))
    )
  )

;; storing result of step8 in a list for further calculations
(define step9 (border_points acc1))

;;(displayln step9)

;;Writing result of Step9 to Output port
(displayln step9 outPort_1)

;;;;; Step9 Over ;;;;;

;;;;; Step10 Begins ;;;;;

;; temporary lists to store previous results
(define temp_border_points step9)
;;(define temp_core_points step6)

#|(define (border_point_clustering b l1 b_r)
  (cond
    ((list? null) b_r)
    ((= (common_elements (move_to_list step3 b) (move_to_list step3 (car l1))) (common_elements (move_to_list step3 b) (move_to_list step3 b_r))) (border_point_clustering b (cdr l1) (min (car l1) b_r)))
    ((> (common_elements (move_to_list step3 b) (move_to_list step3 (car l1))) (common_elements (move_to_list step3 b) (move_to_list step3 b_r))) (border_point_clustering b (cdr l1) (car l1)))
    (else (border_point_clustering b (cdr l1) b_r))
    )
  )|#

;;(border_point_clustering 19 temp_core_points (car temp_core_points))
#|
(define (border_clustering b l1 x)
  (cond
    (() (car l1))
    (else (border_clustering (cdr l1)))
    )
  )
;;(border_clustering 2 (move_to_list step3 2) (car (move_to_list step3 2)))
(move_to_list step3 2)
(display step6)|#
;;;;; Step10 Over ;;;;;
;;Closing output port to close output file in use
(close-output-port outPort_1)
#|(move_to_list step4 5)
(sort(list_for_clusters (move_to_list step4 29)) <)|#