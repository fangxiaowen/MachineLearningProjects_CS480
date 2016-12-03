;;;;HW6. 

;;; Some utility Functions and Macros that you might find to be useful (hint)
(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

;;; Example usage
;;;
;;; (let ((x 0))
;;;    (while (< x 5)
;;;        (print x)
;;;        (incf x)))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))

;; hope this works right
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))


;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS

;;; TOURNAMENT SELECTION

;; is this a good setting?  Try tweaking it (any integer >= 2) and see
(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  ;;; IMPLEMENT ME
  ;;;
  ;;; See Algorithm 32 of Essentials of Metaheuristics
	(let ((best (elt population (setf index-best (random (length population))))))	
		(dotimes (x (- *tournament-size* 1))	
			(setf next (elt population (setf index-next (random (length population)))))
			(if (> (elt fitnesses index-next) (elt fitnesses index-best)) (progn (setf best next) (setf index-best index-next)))	
		)
		best
	)

)


(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"

  ;;; IMPLEMENT ME
  ;;;
  ;;; Hint: This is a very short function.  Maybe one of the
  ;;; Utility functions I provided might be of benefit here
	(generate-list num (lambda () (tournament-select-one population fitnesses)))
)



;; I'm nice and am providing this for you.  :-)
(defun simple-printer (population fitnesses)
  "Determines the individual in population with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit))) population fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))



(defun evolve (generations pop-size &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size POP-SIZE, using various functions"
  ;;; HINTS: You could do this in many ways.  But I implemented it using
  ;;; the following functions (among others)
  ;;;
  ;;; FUNCALL FORMAT MAPCAR LAMBDA APPLY
	(funcall setup)
			
	(setf P (generate-list pop-size creator))
	(dotimes (i generations) 
		(setf fit (mapcar #'(lambda(x) (funcall evaluator x)) P)) ;; fitness of each element
		(setf best-fit (apply 'max fit)) ;;the best fitness
		(setf best-candidate (find-if #'(lambda (x) (= (funcall evaluator x) best-fit)) P))	;;find the candidate with best fitness	
		(if (> best-fit (funcall evaluator best)) (setf best best-candidate)) ;; determine the best one
	
		;; generate next generation		
		(setf Q nil)			
		(dotimes (y (/ pop-size 2))
			(setf p1 (elt (funcall selector pop-size P fit) (random pop-size))) 
			
			(setf p2 (elt (funcall selector pop-size P fit) (random pop-size))) 
			(setf children (funcall modifier p1 p2))
			(push (first children) Q)
			(push (second children) Q)			
		)
		(setf old-P P)
		(setf P Q)
		;;print this generation
		(funcall printer old-P fit)
	)
	;;best
)




;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM


;;; Here you will implement creator, modifier, and setup functions for
;;; individuals in the form of lists of floating-point values.  
;;; I have provided some objective functions which you can use as
;;; fitness evaluation functions.

;;; If you were really into this, you might try implementing an evolution
;;; strategy instead of a genetic algorithm and compare the two.
;;;
;;; If you were really REALLY into this, I have an extension of this
;;; project which also does genetic programming as well.  That is a much
;;; MUCH MUCH MUCH more difficult project.


(defparameter *float-vector-length* 20 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")

(defun random-float ()
	(- *float-max* (random (- *float-max* *float-min*))))

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with UNIFORM random numbers in the range appropriate to the given problem"

  ;;; IMPLEMENT ME
  ;;;
  ;;; The numbers must be uniformly randomly chosen between *float-min* and
  ;;; *float-max*.  See the documentation for the RANDOM function.

  ;;; HINT: Maybe a function I provided in the utilities might
  ;;; be handy here
	(generate-list *float-vector-length* #'random-float) 	
)

;; I just made up these numbers, you'll probably need to tweak them
(defparameter *crossover-probability* 0.05
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.1
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.008
  "Per-gene mutation variance in gaussian convolution")

(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.  
The individuals are guaranteed to be the same length.  Returns NIL."

  ;;; IMPLEMENT ME
  ;;;
  ;;; For crossover: use uniform crossover (Algorithm 25) in
  ;;;                Essentials of Metaheuristics
  ;;; HINTS:
  ;;; DOTIMES, ELT, and ROTATEF
	(dotimes (x (length ind1))
		(if (random? *crossover-probability*) (rotatef (elt ind1 x) (elt ind2 x)))
		
	)
	
)


(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."

  ;;; IMPLEMENT ME
  ;;;
  ;;; For mutation, see gaussian convolution (Algorithm 11) in
  ;;;                Essentials of Metaheuristics
  ;;; Keep in mind the legal minimum and maximum values for numbers.
  ;;; HINTS:
  ;;; Maybe a function or three in the utility functions above might be handy
  ;;; See also SETF
	(dotimes (x (length ind) nil)
		(setf n (gaussian-random 0 *mutation-variance*))
		(if (random? *crossover-probability*)  
			(progn (while (or (< (+ n (elt ind x)) *float-min*) (> (+ n (elt ind x)) *float-max*)) 
				(setf n (gaussian-random 0 *mutation-variance*))
				)
				(setf (elt ind x) (+ n (elt ind x)))
			)
		)
		
	)
		

)


(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

    ;;; IMPLEMENT ME
    ;;; It's pretty straightforward.
    ;;; This function should first COPY the two individuals, then
    ;;; CROSS THEM OVER, then mutate the result using gaussian covolution,
    ;;; then return BOTH children together as a list (child1 child2)
    ;;;
    ;;; HINTS:
    ;;; For copying lists:  See the Lisp Cheat Sheet 
    ;;;                (http://cs.gmu.edu/~sean/lisp/LispCheatSheet.txt)
	(setf child1 (copy-seq ind1))
	(setf child2 (copy-seq ind2))
	(funcall #'uniform-crossover child1 child2)
	(funcall #'gaussian-convolution child1)	
	(funcall #'gaussian-convolution child2)
	(list child1 child2)
)

;; you probably don't need to implement anything here
(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
	(setf best (float-vector-creator))
)



;;; FITNESS EVALUATION FUNCTIONS

;;; I'm providing you with some classic objective functions.  See section 11.2.2 of
;;; Essentials of Metaheuristics for details on these functions.
;;;
;;; Many of these functions (sphere, rosenbrock, rastrigin, schwefel) are
;;; traditionally minimized rather than maximized.  We're assuming that higher
;;; values are "fitter" in this class, so I have taken the liberty of converting
;;; all the minimization functions into maximization functions by negating their
;;; outputs.  This means that you'll see a lot of negative values and that's fine;
;;; just remember that higher is always better.
;;; 
;;; These functions also traditionally operate with different bounds on the
;;; minimum and maximum values of the numbers in the individuals' vectors.  
;;; Let's assume that for all of these functions, these values can legally
;;; range from -5.12 to 5.12 inclusive.  One function (schwefel) normally goes from
;;; about -511 to +512, so if you look at the code you can see I'm multiplying
;;; the values by 100 to properly scale it so it now uses -5.12 to 5.12.

(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
			   (+ (* (- 1 x) (- 1 x))
			      (* 100 (- x1 (* x x)) (- x1 (* x x)))))
			 ind (rest ind)))))

(defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
	(reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
			    ind)))))

(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	
			 (mapcar (lambda (x) (* x 100)) ind)))))



