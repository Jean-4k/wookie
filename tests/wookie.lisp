(in-package :wookie/tests)



(plan 5)



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



(subtest "make-wookie-state-hooks"
  (let ((hash-table (wookie::make-wookie-state-hooks)))
    (is (type-of hash-table)
	'hash-table)
    (is (hash-table-test hash-table)
	'eq)))



(subtest "make-wookie-state without hooks."
  (let ((hooks (wookie-state-hooks
		(wookie::make-wookie-state))))
    (if (eq (type-of hooks) 'hash-table)
	(is (hash-table-test hooks)
	    'eq)
	(fail "hooks must be a hash-table."))))



(subtest "wookie-state hooks initform is an error."
  (is-error (make-instance 'wookie-state)
	    'wookie-state-hooks-not-set))
