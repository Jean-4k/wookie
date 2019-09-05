(in-package :wookie/tests)



(plan 2)



(subtest "*state* is an instance of wookie-state."
  (is (type-of *state*)
      'wookie-state))



(subtest "make-wookie-state"
  (let ((state (wookie::make-wookie-state)))
    (is (type-of state)
	'wookie-state)))
