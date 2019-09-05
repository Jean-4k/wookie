(in-package :wookie/tests)



(plan 3)



(subtest "*state* is an instance of wookie-state."
  (is (type-of *state*)
      'wookie-state))



(subtest "make-wookie-state"
  (let ((state (wookie::make-wookie-state)))
    (is (type-of state)
	'wookie-state)))



(subtest "make-wookie-state with hooks."
  (let* ((hooks (make-hash-table :test #'eq))
	 (state))
    (setf (gethash :test1 hooks) "value1")
    (setf (gethash :test2 hooks) "value2")
    (setf state (wookie::make-wookie-state :hooks hooks))

    (let ((gotten-hooks (wookie-state-hooks state)))
      (is (gethash :test1 gotten-hooks)
	  (gethash :test1 hooks))
      (is (gethash :test2 gotten-hooks)
	  (gethash :test2 hooks)))))
	  
