(in-package :dlist.test)

(def-suite dlist-test-suite
    :description "The double linked lists test suite")

(defun run-tests ()
  (run 'dlist-test-suite))

(in-suite dlist-test-suite)

(test creation-test
  (let ((dlist (make-dlist)))
    (is (null-dlist dlist))))

(test insertion-test
  (let ((dlist (make-dlist)))
    (insert-head dlist 1)
    (is (equalp (dlist-elements dlist) '(1)))
    (insert-tail dlist 4)
    (is (equalp (dlist-elements dlist) '(1 4)))
    (insert-after dlist (dlist-head dlist) 2)
    (is (equalp (dlist-elements dlist) '(1 2 4)))))

(test removal-test
  (let ((dlist (make-dlist)))
    (insert-head dlist 1)
    (insert-tail dlist 4)
    (let ((rem-link (insert-before dlist (dlist-tail dlist) 3)))
      (is (equalp (dlist-elements dlist) '(1 3 4)))
      (remove-link dlist rem-link)
      (is (equalp (dlist-elements dlist) '(1 4)))
      (remove-link dlist rem-link)
      (is (equalp (dlist-elements dlist) '(1 4))))))

(test mapping-test
  (let ((dlist (make-dlist)))
    (insert-head dlist 1)
    (insert-tail dlist 4)
    (insert-after dlist (dlist-head dlist) 2)
    (is (equalp (map-dlist #'1+ dlist) '(2 3 5)))))

(test position-test
  (let* ((dlist (make-dlist))
	 (dlink1 (insert-head dlist 1))
	 (dlink2 (insert-tail dlist 2)))
    (is (equalp (dlist-position dlink1 dlist) 0))
    (is (equalp (dlist-position dlink2 dlist) 1))
    (remove-link dlist dlink1)
    (is (null (dlist-position dlink1 dlist)))
    (is (equalp (dlist-position dlink2 dlist) 0))))
