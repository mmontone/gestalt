(in-package :cl-url.test)

(def-suite cl-url-tests)

(in-suite cl-url-tests)

(defun run-tests ()
  (run 'cl-url-tests))

;reader-syntax:   #url"http://localhost/myapp/main.do"

(test url-reader-syntax-test
  (is (url-equal #u"/" (make-url)))
  (is (url-equal #u"" (make-url)))
  (is (url-equal #u"//localhost" (make-url :hostname "localhost")))
  (is (url-equal #u"foo/bar" (make-url :path (list "foo")
				       :program "bar")))
  (is (url-equal #u"bar/" (make-url :path (list "bar"))))
  (is (url-equal #u"foo/bar/" (make-url :path (list "foo" "bar"))))
  (is (url-equal #u"/foo/bar/" (make-url :path (list "foo" "bar"))))
  (is (url-equal #u"#fragment" (make-url :fragment "fragment")))
  (is (url-equal #u"action.do" (make-url :program "action.do")))
  (is (url-equal #u"http:" (make-url :protocol :http)))
  (is (url-equal #u"?a1=v1&a2=v2" (make-url :parameters '(:a1 v1 :a2 v2))))
  (is (url-equal #u"//:80" (make-url :port 80)))
  (is (url-equal #u"//localhost:80" (make-url :hostname "localhost"
					       :port 80))))






  