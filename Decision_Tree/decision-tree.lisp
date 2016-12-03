;;;HW2-Decision Tree

(let ((unique (gensym)))
(defun build-decision-tree (examples feature-defs label &optional (default unique)) 
	
	;; so this is setting default label
	(setf default (most-common-label examples))
    ;; okay, now your code goes here....
	
	;; examples is null	
	(if (null examples) (return-from build-decision-tree (append '(:LABEL) (list default))))
	;; all samples have the same label
	(if (examples-have-same-label-p examples) (return-from build-decision-tree (append '(:LABEL) (list default))))
	;;features is null or same value & diff label
	(if (or (null feature-defs) (and (null (examples-have-same-label-p examples)) (= 1 (list-length (remove-duplicates examples))) )) (return-from build-decision-tree (append '(:LABEL) (list default))))
	;; time to pick a feature and build a node based on it
		;; choose the feature with least remainder
	(setf f (choose-feature feature-defs examples label))
		;; node with feature *f*
		;;build subtree on *tree*
		;; iterate every value in this feature
	(setf f-name (first f))
	(setf f-values (last f (- (length f) 1)))
	(let ((tree (list f-name)))
	(dolist (v  f-values)
		;; examples with this feature value
		(setf answers (examples-with-feature-val examples f-name v))
		;; reduce this feature
		(setf reduced-features (remove f feature-defs))
		;; labels of these examples 
		(setf answer-labels (mapcar #'label-of-example answers))
		;; *child* is the subtree we build
		(setf child (build-decision-tree answers reduced-features answer-labels default )) 		
			;; add *child* as a child subtree of *tree*, with a  query of = *v*?
		(setf tree (append tree (list (list v child))))
		)	
	tree)		
))

(defun choose-feature (feature-defs examples label)
	;;the remainder list of all features
	(setf *remainders* (mapcar #'(lambda (feature-def) (remainder feature-def examples label)) feature-defs))	
	;; the lowest reminder
	(elt feature-defs (position (reduce #'min *remainders*) *remainders*)))


(defun information (probabilities)
	(- (apply #'+ (mapcar #'(lambda (x) (if (= x 0) 0 (* x (log x 2)))) probabilities)))
)


(defun remainder (feature-def examples label)
	;;feature name
	(setf *f-name* (first feature-def))
	;;feature values
	(setf *f-values* (last feature-def (- (length feature-def) 1) ))
	(setf info-entrophy (mapcar #'(lambda (value)  (information (label-probabilities-for-feature-val examples *f-name* value label) )) *f-values*))
	(setf weight (mapcar #'(lambda (value) (/ (num-examples-with-feature-val examples *f-name* value) (length examples))) *f-values*))	
	(apply #'+ (mapcar #'* info-entrophy weight))
)


(defun label-probabilities-for-feature-val (examples feature value label)
  (let ((examples-with-feature-val
	 (if (null feature) examples 
	   (remove-if-not #'(lambda (x) (equalp 
					 (feature-val-for-example x feature) value)) examples))))
    (normalize
     (mapcar #'(lambda (lab) (/ 
				(num-examples-with-label examples-with-feature-val lab) 
				(length label)))
	     label))))


(defun examples-have-same-label-p (examples)
  "Returns t if all examples have the same label, else returns nil"
  (let ((label (label-of-example (first examples))))
    (dolist (example examples t)
      (when (not (equalp label (label-of-example example)))
	(return nil)))))



(defun feature-val-for-example (example feature)
  "Returns the value of a given feature as stored in an example"
  (second (assoc feature example)))


(defun label-of-example (example)
  "Returns the label of an example, as stored in the example"
  (second (assoc :label example)))


(defun most-common-label (examples)
  "Returns the most common label found in examples"
  (most-common (mapcar #'label-of-example examples)))


(defun find-label-for-example (example decision-tree)
  "Given a decision tree and an example, uses the decision tree
    to determine the expected label for the example"
  (if (equalp (first decision-tree) :label)
      (second decision-tree) ;; return the label
    (find-label-for-example example
			    (second (assoc
				     (feature-val-for-example example (first decision-tree))
				     (rest decision-tree))))))


(defun num-examples-with-label (examples label)
  "Returns the number of examples with a given label"
  (count-if #'(lambda (example) (equalp 
				 (label-of-example example) label)) examples))


(defun num-examples-with-feature-val (examples feature value)
  "Returns the number of examples with a given value for some feature"
  (count-if #'(lambda (example) (equalp
				 (feature-val-for-example example feature) value)) examples))


(defun examples-with-feature-val (examples feature val)
  "Returns the examples whose feature has value val"
  (remove-if-not #'(lambda (x) (equalp (feature-val-for-example x feature) val)) examples))



(defun most-common (elts &key (test #'equalp))
  "Returns the elt in the list elts that appears the most often;
     ties are broken arbitrarily, probably by picking the elt 
     which appeared earliest. Two elts are considered to be
     the same if they pass the :test (by default, equalp)"
  (let* ((unique-elts (remove-duplicates elts :test test))
	 (unique-nums (mapcar #'(lambda (x) (count x elts :test test)) unique-elts))
	 (best-elt (first unique-elts))
	 (best-num (first unique-nums)))
    (mapc #'(lambda (elt num) (when (> num best-num) 
				(setf best-elt elt) (setf best-num num)))
	  (rest unique-elts) (rest unique-nums))
    best-elt))


(defun normalize (numbers)
  "Normalizes a list of numbers by dividing them by their sum.  If
    the numbers are all 0.0, normalize just returns the original numbers."
  (let ((sum (apply #'+ numbers)))
    (if (= sum 0.0) numbers 
      (mapcar #'(lambda (num) (/ num sum)) numbers))))


(build-decision-tree *restaurant-examples*  *restaurant-feature-defs* *restaurant-labels*)



(setf *t* (build-decision-tree *restaurant-trial-examples* *restaurant-feature-defs* *restaurant-labels*))

(mapcar #'label-of-example *restaurant-examples*)

(mapcar #'(lambda (x) (find-label-for-example x *t*)) *restaurant-examples*)


(defparameter *restaurant-labels*
  '(t nil)
  "label Values for the Restaurant problem")

(defparameter *restaurant-feature-defs*
  '((alternative t nil)
    (has-bar t nil)
    (open-friday t nil)
    (hungry t nil)
    (patrons none some full)
    (price cheap medium expensive)
    (raining t nil)
    (reservation t nil)
    (type french thai burger italian)
    (estimated-wait-time 0-10 10-30 30-60 >60))    
  "A list of feature-defs for the Restaurant problem.
    feature defs take the form (feature val1 val2 val3 ...)")


(defparameter *restaurant-examples*
  '(((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time 0-10) (:label t))
    ((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 30-60) (:label nil))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons some) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label t))
    ((alternative t) (has-bar nil) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 10-30) (:label t))
    ((alternative t) (has-bar nil) (open-friday t) (hungry nil) (patrons full) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time >60) (:label nil))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type italian) (estimated-wait-time 0-10) (:label t))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining t) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label nil))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type thai) (estimated-wait-time 0-10) (:label t))
    ((alternative nil) (has-bar t) (open-friday t) (hungry nil) (patrons full) (price cheap) (raining t) (reservation nil) (type burger) (estimated-wait-time >60) (:label nil))
    ((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price expensive) (raining nil) (reservation t) (type italian) (estimated-wait-time 10-30) (:label nil))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 0-10) (:label nil))
    ((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 30-60) (:label t)))
  "A list of examples for the Restaurant problem.
    Examples take the form of ((attr1 val) (attr2 val) ... (:label label))")


(defparameter *restaurant-trial-examples*
  '(((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 30-60) (:label nil))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons some) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label t))
    ((alternative t) (has-bar nil) (open-friday t) (hungry nil) (patrons full) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time >60) (:label nil))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type thai) (estimated-wait-time 0-10) (:label t))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 0-10) (:label nil))
    ((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 30-60) (:label t)))
  "Some training-case examples for the Restaurant problem, selected from
    the full set of examples above. 
    Examples take the form of ((attr1 val) (attr2 val) ... (:label label))")  


;;;This is for mapping dataset in HW3
(defun map-dataset (feature-name example)
(append (mapcar #'list feature-name  (first example))  (list (append '(:LABEL) (first (last example)))))
)

(defparameter *vote-feature-name* '(fn1 fn2 fn3 fn4 fn5 fn6 fn7 fn8 fn9 fn10 fn11 fn12 fn13 fn14 fn15 fn16))

(defparameter *vote-record* '((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.1 0.9) (0.1)))

(map-dataset *vote-feature-name* *vote-record*)




;;; 3  wrong labels

;;;Here is the 500-word report
;;How did I work on this hw?
;;I follow the sepecification, writing the frame of build-decision-tree first and then othe functions. I use presudocode to write them. Then just copy and paste the auxilary functions provided by Prof Sean to where they should be. This copy and paste step let me realized how important to modulize programs. 
;;Another practice is modifing the variables to the format suitable for other function. I modify feature-def to f-name only containing the feature name and f-values only containing feature values. 
;;And decision tree is a tree. We can build it recursively. So we need use local variables for variables passed recursively.

;;What did I learn from this model?
;;Decision tree is a pretty straightforward method. The essence of it is choosing features. We use information entrophy as the measurement of feature quility.   
;;Because decision tree is for building the query to get clearest and simplest answer, so we need to choose the query giving us clearest and simplest answer, which is choosing the feature with lowest info entrophy.
;;This cannot promise global optima, though. But we don't need to worry about that for this problem.
;;This will overfitting the problem. Because our return condition is strict.In real world, there are many noises. So we shouldn't be that strict about return condition. We could set a threshhold. If remainder is under this threshold, we can return.
;;But we don't have such threshold. Thus we have ugly decision tree and 5 wrong prediction, 5/12! While this is also due to too small training set.  
  
  

