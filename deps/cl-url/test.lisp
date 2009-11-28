(in-package :cl-url.test)

(def-suite cl-url-tests)

(in-suite cl-url-tests)

(defun run-test ()
  (run 'cl-url-tests))

;reader-syntax:   #url"http://localhost/myapp/main.do"

#|
(test url-reader-syntax-test
  (is (equal-url #url"/" (make-url)))
  (is (equal-url #url"" (make-url)))
  (is (equal-url #url"//localhost") (make-url :hostname "localhost"))
  (is (equal-url #url"foo/bar") (make-url :path (list "foo")
					  :program "bar"))
  (is (equal-url #url"bar/") (make-url :path (list "bar")))
  (is (equal-url #url"foo/bar/" (make-url :path (list "foo" "bar"))))
  (is (equal-url #url"/foo/bar/" (make-url :path (list "foo" "bar"))))
  (is (equal-url #url"#fragment" (make-url :fragment "fragment")))
  (is (equal-url #url"action.do") (make-url :program "action.do"))
  (is (equal-url #url"http:") (make-url :protocol :http))
  (is (equal-url #url"?a1=v1&a2=v2" (make-url :parameters '(:a1 v1 :a2 v2))))
  (is (equal-url #url":80") (make-url :port 80))
  (is (equal-url #url"//localhost:80") (make-url :hostname "localhost"
						 :port 80)))

|#



  