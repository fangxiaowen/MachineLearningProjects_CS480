;;;HW1 of cs480


;;;Use equalp to test type and equal
;;;3 other functions to compute difference
(defun difference (list1 list2 &key (measure 'count))
	(if (equalp measure 'count) (funcall #'diff_Count list1 list2) (if (equalp measure 'squared) (funcall #'diff_Squared list1 list2) (funcall #'diff_Manhattan list1 list2)))) 


;;;Use MOST-COMMON to select the most common class between the 2 classes
(defun k-nearest-neighbor (examples new-example &key (k 1) (measure 'count))
	;;append the difference to examples
	(setf *new* (mapcar #'cons (mapcar #'(lambda (x) (difference (first x) (first new-example) :measure measure)) examples) examples))
	;;the k most close examples
	(setf *close* (subseq (sort *new* #'less_list) 0 k))
	;;class of the k most close examples
	(setf close_class (mapcar #'(lambda (li) (first (last li))) *close*))
	;;find the most common class
	(most-common close_class))

;;;Easy
(defun generalization (training-example test-examples &key (k 1) (measure 'count))
	(setf predict (mapcar #'(lambda (x) (k-nearest-neighbor training-example x :k k :measure measure)) test-examples))
	(setf result (mapcar #'equalp predict (mapcar #'first (mapcar #'last test-examples))))
	(/ (count-if #'(lambda (i) (equalp i T))result) (length test-examples)))





;;;Count different elements to compute distance
(defun diff_Count (list1 list2)
 (setf *k* 0)
 (dolist (x (mapcar #'equalp list1 list2))
 (if (equalp x NIL) (incf *k*)))
  *k*)

;;;Suqared sum to compute distance
(defun diff_Squared (list1 list2)
(apply '+ (mapcar #'* (mapcar #'- list1 list2) (mapcar #'- list1 list2))))


;;;Manhattan distance
(defun diff_Manhattan (list1 list2)
(apply '+ (mapcar #'abs (mapcar #'- list1 list2)))) 





;;;Most-common function
(defun most-common (list)
	(let ((reduced-elts (mapcar (lambda (elt) (list elt (count elt list))) (remove-duplicates list) ) ))
	(first (first (sort reduced-elts #'> :key #'second))))) 

;;;Comparator for 2 lists
(defun less_list (list1 list2)
	(< (first list1) (first list2)))







;;;Test result of the two examples:
;;;(0.9) for the first example and 87/94 for the second example.







;;;500-WORD REPORT
;;;How did I implement this task? 
;;;Following the normal program development practice, first I write the interface of the three functions. Basically just copy the function prototype provided by the assignment. Second, I write pseudocode to describe processes of each function. Then I have to transfer them into lisp, looking up "lisp quickstart" many times. During this process, I learned how to use lambda expression and list manipulation, understanding what "programable programming" means somehow. And lisp is consice, which saves much coding time. 
;;;How about playing with the test data?
;;;For *Voting-Records-Short* data, K-nearest-neighbor is a good model. I guess it is because the feature space is small--every feature is in a small domain, (0.9, 0.1, 0.5). And all the features are normalized. So COUNT, SQUARE and MANHATTAN can all capture the difference very well. As for the ratio between trainning examples and testing example, as I decrease the trainning set and increase the testing set, the outcome is decreasing. I guess it's because some really close "neighbor" lost from training set. But the decrease in outcome is not obvious.It could be underfitting, but not overfitting. Overall, k-nearest-neighbor works well for this data set!
;;;For *wine*, it works badly. I have several guesses. First, the last feature is not normalized. So SQUARE and MANHATTAN won't perform very well. Second, most features are real value. So COUNT won't perform very well.
;;;For *yeast*, it also performs badly. All features are real value and too small and too similar with each other, which makes computing difference useless. And too many classes. k-nearest-neighbor might not be suitable for multi-class data.
;;;For *tae*, it also performs badly. I think it is because teaching performance depends on how responsible, how passionate, how smart and other qualities in a person. And such qualities have little to do with the features in the data set except Feature 1 (English speaker).

;;;Xiaowen Fang             
