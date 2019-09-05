(in-package :wookie/tests)



(plan 1)



(subtest "*state* is an instance of wookie-state."
  (is (type-of *state*)
      'wookie-state))






