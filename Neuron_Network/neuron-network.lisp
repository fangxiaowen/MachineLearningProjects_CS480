;;;HW5--Neron Network

(defun sigmoid (u)
  "Sigmoid function applied to the number u"
	(/ 1 (+ 1 (exp (- u))))	  
)


;; output and correct-output are both column-vectors
(defun net-error (output correct-output)
  "Returns (as a scalar value) the error between the output and correct vectors"
	(first (first (scalar-multiply 1/2 (multiply (transpose output) correct-output))))	  
)

(defun forward-propagate (datum v w)
  "Returns as a vector the output of the OUTPUT units when presented
the datum as input."
	;; datum as input, v and w are weight matrix
	(setf hidden (map-m #'sigmoid (multiply v (first datum))))
	(setf outcome (map-m #'sigmoid (multiply w hidden)))
)


(defun back-propagate (datum alpha v w)
  "Back-propagates a datum through the V and W matrices,
returning a list consisting of new, modified V and W matrices."
  ;; Consider using let*
  ;; let* is like let, except that it lets you initialize local
  ;; variables in the context of earlier local variables in the
  ;; same let* statement.
	
	;;compute hidden layer 
	(let* (		(correct (second datum))
			(hidden (map-m #'sigmoid (multiply v (first datum))))
			;; compute outcome
			(outcome (map-m #'sigmoid (multiply w hidden)))
			;;Now let's backpropagate!
			;; d,e is a temporary matrix
			 (d  (e-multiply (subtract outcome correct) outcome (subtract-from-scalar 1 outcome )))
			(delta-w (scalar-multiply alpha (multiply d (transpose hidden))))
			(e (e-multiply hidden (subtract-from-scalar 1 hidden) (multiply (transpose w) d) ))
			(delta-v (scalar-multiply alpha (multiply e (transpose (first datum)))))
			;;update v and w (weights)
			(v (add v delta-v))
			(w (add w delta-w)))
			;;return v and w
			(list v w))
			;; (print v-and-w)	
)


(defun net-build (data num-hidden-units alpha initial-bounds max-iterations modulo &optional print-all-errors)
  "Then performs the following loop MAX-ITERATIONS times, or until the error condition	is met (see below):
The function should return a list of two items: the final V matrix
and the final W matrix of the learned network."
	;;preprocessing raw data
	;;(setf data (convert-data data))	
	;;(shuffle data)
	;;size of input, or num of colums of v
	(setf size-input (length (first (first data))))
	;;size of output, or num of rows of w
	(setf size-output (length (second (first data))))
	;;randomlize v and w
	(setf v (make-random-matrix num-hidden-units size-input initial-bounds)) 
	(setf w (make-random-matrix size-output num-hidden-units initial-bounds))
	
	;;now let's loop!
	(dotimes (looptime max-iterations)
		;;shuffle data
		(shuffle data)
		
		;;let's back-propagate for all inputs!!
		(dotimes (index size-input)
			(let ((vw (back-propagate (elt data index) alpha v w)))
                         (setf v (first vw))
			 (setf w (second vw))  
			))
		(if (= 0 (mod looptime modulo)) 
			(let* ((errors (mapcar #'(lambda (element) (net-error (forward-propagate element v w) (second element))) 
			data))
				
				(worst-error (apply #'max errors))
				(ave-error (average errors)))
				(if (eq t print-all-errors) (progn (print "these are errors for every input in this data set") (print errors)))
				(print "these are worst error and average error in this iteration")
				(print worst-error)
				(print ave-error)
				(if (< worst-error *a-good-minimum-error*) (return-from net-build (list v w)))
			)
		)
	)
	(list v w)	
)


(defun simple-generalization (data num-hidden-units alpha initial-bounds max-iterations)
  "Given a set of data, trains a neural network on the first half
of the data, then tests generalization on the second half, returning
the average error among the samples in the second half.  Don't print any errors,
and use a modulo of MAX-ITERATIONS."
	;;scale our data first!
	(setf data (scale-data data))
	;;get the first half and last half data
	(setf datum-data (convert-data data))
	(setf data-size (length datum-data))
	(setf first-half (butlast datum-data (floor (/ data-size 2))))
	(setf last-half (last datum-data (ceiling (/ data-size 2))))
	;;build neron-network and test!
	(let* ( (network (net-build first-half num-hidden-units alpha initial-bounds max-iterations max-iterations nil))
		(v (first network))
		(w (second network))
		(test-errors (mapcar #'(lambda (test-element) (net-error (forward-propagate test-element v w) (second test-element))) last-half))
		(mean-errors (average test-errors))  )
		(print "this is the mean error of test set!")
		(print mean-errors)
			
	)
)

;; Basic Operations

(defun map-m (function &rest matrices)
  "Maps function over elements in matrices, returning a new matrix"
  (apply #'verify-equal 'map-m  matrices)
  (apply #'mapcar #'(lambda (&rest vectors)       ;; for each matrix...
		      (apply #'mapcar #'(lambda (&rest elts)     ;; for each vector...
					  (apply function elts))
			     vectors)) 
	 matrices))   ;; pretty :-)

(defun transpose (matrix)
  "Transposes a matrix"
  (apply #'mapcar #'list matrix))  ;; cool, no?

(defun make-matrix (i j func)
  "Builds a matrix with i rows and j columns,
    with each element initialized by calling (func)"
  (map-m func (make-list i :initial-element (make-list j :initial-element nil))))

(defun make-random-matrix (i j val)
  "Builds a matrix with i rows and j columns,
    with each element initialized to a random
    floating-point number between -val and val"
  (make-matrix i j #'(lambda (x)
		       (declare (ignore x))  ;; quiets warnings about x not being used
		       (- (random (* 2.0 val)) val))))

(defun e (matrix i j)
  "Returns the element at row i and column j in matrix"
  ;; 1-based, not zero-based.  This is because it's traditional
  ;; for the top-left element in a matrix to be element (1,1),
  ;; NOT (0,0).  Sorry about that.  :-)
  (elt (elt matrix (1- i)) (1- j)))

(defun print-matrix (matrix)
  "Prints a matrix in a pleasing form, then returns matrix"
  (mapcar #'(lambda (vector) (format t "~%~{~8,4,,F~}" vector)) matrix) matrix)

;;; Matrix Multiplication

(defun multiply2 (matrix1 matrix2)
  "Multiplies matrix1 by matrix2 
    -- don't use this, use multiply instead"
  (verify-multiplicable matrix1 matrix2)
  (let ((tmatrix2 (transpose matrix2)))
    (mapcar #'(lambda (vector1)
		(mapcar #'(lambda (vector2)
			    (apply #'+ (mapcar #'* vector1 vector2))) tmatrix2))
	    matrix1)))  ;; pretty :-)

(defun multiply (matrix1 matrix2 &rest matrices)
  "Multiplies matrices together"
  (reduce #'multiply2 (cons matrix1 (cons matrix2 matrices))))

;;; Element-by-element operations

(defun add (matrix1 matrix2 &rest matrices)
  "Adds matrices together, returning a new matrix"
  (apply #'verify-equal 'add matrix1 matrix2 matrices)
  (apply #'map-m #'+ matrix1 matrix2 matrices))

(defun e-multiply (matrix1 matrix2 &rest matrices)
  "Multiplies corresponding elements in matrices together, 
        returning a new matrix"
  (apply #'verify-equal 'e-multiply matrix1 matrix2 matrices)
  (apply #'map-m #'* matrix1 matrix2 matrices))

(defun subtract (matrix1 matrix2 &rest matrices)
  "Subtracts matrices from the first matrix, returning a new matrix."
  (let ((all (cons matrix1 (cons matrix2 matrices))))
    (apply #'verify-equal 'subtract all)
    (apply #'map-m #'- all)))

(defun scalar-add (scalar matrix)
  "Adds scalar to each element in matrix, returning a new matrix"
  (map-m #'(lambda (elt) (+ scalar elt)) matrix))

(defun scalar-multiply (scalar matrix)
  "Multiplies each element in matrix by scalar, returning a new matrix"
  (map-m #'(lambda (elt) (* scalar elt)) matrix))

;;; This function could
;;; be done trivially with (scalar-add scalar (scalar-multiply -1 matrix))
(defun subtract-from-scalar (scalar matrix)
  "Subtracts each element in the matrix from scalar, returning a new matrix"
  (map-m #'(lambda (elt) (- scalar elt)) matrix))




;;;; Some useful preprocessing functions

(defun scale-list (lis)
  "Scales a list so the minimum value is 0.1 and the maximum value is 0.9.  Don't use this function, it's just used by scale-data."
  (let ((min (reduce #'min lis))
	(max (reduce #'max lis)))
    (mapcar (lambda (elt) (+ 0.1 (* 0.8 (/ (- elt min) (- max min)))))
	    lis)))

(defun scale-data (lis)
  "Scales all the attributes in a list of samples of the form ((attributes) (outputs))"
  (transpose (list (transpose (mapcar #'scale-list (transpose (mapcar #'first lis))))
		   (transpose (mapcar #'scale-list (transpose (mapcar #'second lis)))))))

(defun convert-data (raw-data)
  "Converts raw data into column-vector data of the form that
can be fed into NET-LEARN.  Also adds a bias unit of 0.5 to the input."
  (mapcar #'(lambda (datum)
	      (mapcar #'(lambda (vec)
			  (mapcar #'list vec))
		      (list (cons 0.5 (first datum))
			    (second datum))))
	  raw-data))

(defun average (lis)
  "Computes the average over a list of numbers.  Returns 0 if the list length is 0."
  (if (= (length lis) 0)
      0
      (/ (reduce #'+ lis) (length lis))))

(defun shuffle (lis)
  "Shuffles a list.  Non-destructive.  O(length lis), so
pretty efficient.  Returns the shuffled version of the list."
  (let ((vec (apply #'vector lis)) bag (len (length lis)))
    (dotimes (x len)
      (let ((i (random (- len x))))
	(rotatef (svref vec i) (svref vec (- len x 1)))
	(push (svref vec (- len x 1)) bag)))
    bag))   ;; 65 s-expressions, by the way


(defparameter *verify* t)


;;; hmmm, openmcl keeps signalling an error of a different kind
;;; when I throw an error -- a bug in openmcl?  dunno...
(defun throw-error (str)
  (error (make-condition 'simple-error :format-control str)))

(defun verify-equal (funcname &rest matrices)
  ;; we presume they're rectangular -- else we're REALLY in trouble!
  (when *verify*
    (unless (and
	     (apply #'= (mapcar #'length matrices))
	     (apply #'= (mapcar #'length (mapcar #'first matrices))))
      (throw-error (format t "In ~s, matrix dimensions not equal: ~s"
			   funcname
			   (mapcar #'(lambda (mat) (list (length mat) 'by (length (first mat))))
				   matrices))))))

(defun verify-multiplicable (matrix1 matrix2)
  ;; we presume they're rectangular -- else we're REALLY in trouble!
  (when *verify*
    (if (/= (length (first matrix1)) (length matrix2))
	(throw-error (format t "In multiply, matrix dimensions not valid: ~s"
			     (list (list (length matrix1) 'by (length (first matrix1)))
				   (list (length matrix2) 'by (length (first matrix2)))))))))


(defun optionally-print (x option)
  "If option is t, then prints x, else doesn't print it.
In any case, returns x"
  ;;; perhaps this might be a useful function for you
  (if option (print x) x))

(defun scale-data (lis)
  "Scales all the attributes in a list of samples of the form ((attributes) (outputs))"
  (transpose (list (transpose (mapcar #'scale-list (transpose (mapcar #'first lis))))
		   (transpose (mapcar #'scale-list (transpose (mapcar #'second lis)))))))

(defparameter *a-good-minimum-error* 1.0e-9)

(defparameter *nand*
  '(((0.1 0.1) (0.9))
    ((0.9 0.1) (0.9))
    ((0.1 0.9) (0.9))
    ((0.9 0.9) (0.1))))


(defparameter *xor*
  '(((0.1 0.1) (0.1))
    ((0.9 0.1) (0.9))
    ((0.1 0.9) (0.9))
    ((0.9 0.9) (0.1))))

;;(simple-generalization *voting-records* 4 0.1 1 1000)

;;(multiply (make-random-matrix 4 3 1) (first (first (convert-data *xor*))))

;;(forward-propagate (first (convert-data *xor*)) (make-random-matrix 4 3 1) (make-random-matrix 1 4 1))

;;(back-propagate (first (convert-data *xor*)) 0.1 (make-random-matrix 4 3 1) (make-random-matrix 1 4 1))






;;;500-word report

;;How did I implement this neural network?
;;Proffesor Sean provided all the auxilary functions we need, so we can just presume we have whatever we want and only need to write sigmoid, net-error, forward-propagate, backward-propagate, net-build and simple-generalization.

;;What did I learn from this assignment?
;;I learned that having auxilary functions before is really
;; important.
;;I also learned the power of linear algebra and its significance in CS.
;;And calculus is important as well.
;;And all in all, math capibility is the core competence for CS students. Alon Turing is a mathematitian.
;;As for machine learning itself, it is a little mysterious. We know the basic idea of the algorithm.
;;But why should we use that? why should we choose certain function like sigmoid to simulate the neron? It has math explaination: Non-linear function inNN can composite any differentiatial function in output unit.
;;And sigmoid is easy for back propagation.
;;But NN is getting more and more complex.
;;DNN, RNN, CNN are all very complex. Is this really how our brain works?

;;What did I learn from testing?
;;The parameter really matters.
;;Number of hidden units, Alpha, bounds for randomlization, iteration times.;;The outcome could vary a lot due to different values of auguments.
;;But how to choose the best auguments? No one knows exactly.
;;This inaccuracy makes machine learning less like math and more like statistics.
;;Because of the inaccuracy and uncertainty, I tend to think today's AI and ML industry more like a huge bubble.
;;But sometimes some ML algorithms like DNN or CNN sounds really reasonable.
;;Those algorithms are tring to simulate human's brain. And the way they works sounds really like how our brain works.
;;But not like enough. Cognitive scientists and CS scientists should collarborate more to figure it out.
;;And we need more mathematicians and physicians.   
