(defpackage cl-url.test
  (:use :cl :cl-url :fiveam)
  (:export #:run-tests))

(in-package :cl-url.test)

(def-suite cl-url-tests)

(in-suite cl-url-tests)

(defun run-tests ()
  (run 'cl-url-tests))

;reader-syntax:   #u"http://localhost/myapp/main.do"

(test url-reader-syntax-test
  (is (url-equal #u"/" (make-url)))
  (is (url-equal #u"" (make-url)))
  (is (url-equal #u"//localhost/" (make-url :hostname "localhost")))
  (is (url-equal #u"/foo/bar" (make-url :path (list "foo")
					:program "bar")))
  (is (url-equal #u"/bar/" (make-url :path (list "bar"))))
  (is (url-equal #u"/foo/bar/" (make-url :path (list "foo" "bar"))))
  (is (url-equal #u"#fragment" (make-url :fragment "fragment")))
  (is (url-equal #u"/action.do" (make-url :program "action.do")))
  (is (url-equal #u"http:" (make-url :protocol :http)))
  (is (url-equal #u"?a1=v1&a2=v2" (make-url :parameters '("a1" "v1" "a2" "v2"))))
  (is (url-equal #u"//:80" (make-url :port 80)))
  (is (url-equal #u"//localhost:80" (make-url :hostname "localhost"
					       :port 80))))

(test merge-urls-test
  ;; Test superceeding
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"//remotehost")
		 #u"//localhost"))

  ;; Test use-url conflict resolving
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"//remotehost" :on-conflict :use-url)
		 #u"//localhost"))

  ;; Test use-defaults conflict resolving
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"//remotehost" :on-conflict :use-defaults)
		 #u"//remotehost"))

  ;; Test continue restart
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"//remotehost" :on-conflict :continue)
		 #u""))

  ;; Test error raising
  (signals merge-conflict
	  (merge-urls #u"//localhost" :defaults #u"//remotehost" :on-conflict :error))
  
  ;; Test merging cases
  ;; Protocol against others
  (is (url-equal (merge-urls #u"http:" :defaults #u"//localhost")
		 #u"http://localhost"))
  (is (url-equal (merge-urls #u"http:" :defaults #u"//:80")
		 #u"http://:80"))
  (is (url-equal (merge-urls #u"http:" :defaults #u"/foo/")
		 #u"http:/foo/"))
  (is (url-equal (merge-urls #u"http:" :defaults #u"/foo/bar")
		 #u"http:/foo/bar"))
  (is (url-equal (merge-urls #u"http:" :defaults #u"?foo=bar")
		 (make-url :protocol :http
			   :parameters (list "foo" "bar"))))
  (is (url-equal (merge-urls #u"http:" :defaults #u"#foo")
		 (make-url :protocol :http
			   :fragment "foo")))
  
  ;; Hostname against others
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"http:")
		 #u"http://localhost"))
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"//:80")
		 #u"//localhost:80"))
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"/foo/bar/")
		 #u"//localhost/foo/bar/"))
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"/foo/bar")
		 #u"//localhost/foo/bar"))
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"?foo=bar")
		 #u"//localhost?foo=bar"))
  (is (url-equal (merge-urls #u"//localhost" :defaults #u"#foo")
		 (make-url :hostname "localhost"
			   :fragment "foo")))

  ;; Path against others
  (is (url-equal (merge-urls #u"/foo/bar/" :defaults #u"http:")
		 (make-url :protocol :http
			   :path (list "foo" "bar"))))
  (is (url-equal (merge-urls #u"/foo/bar/" :defaults #u"//localhost")
		 #u"//localhost/foo/bar/"))
  (is (url-equal (merge-urls #u"/foo/bar/" :defaults #u"//:80")
		 #u"//:80/foo/bar/"))
  (is (url-equal (merge-urls #u"/foo/bar/" :defaults #u"/foo/bar")
		 #u"/foo/bar/bar"))
  (is (url-equal (merge-urls #u"/foo/bar/" :defaults #u"?foo=bar")
		 #u"/foo/bar/?foo=bar"))
  (is (url-equal (merge-urls #u"/foo/bar/" :defaults #u"#foo")
		 (make-url :path (list "foo" "bar")
			   :fragment "foo")))

  ;; Program against others
  (is (url-equal (merge-urls #u"/foo" :defaults #u"http:")
		 (make-url :protocol :http
			   :program "foo")))
  (is (url-equal (merge-urls #u"/foo" :defaults #u"//:80")
		 #u"//:80/foo"))
  (is (url-equal (merge-urls #u"/foo" :defaults #u"//localhost")
		 #u"//localhost/foo"))
  (is (url-equal (merge-urls #u"/foo" :defaults #u"/foo/bar/")
		 #u"/foo/bar/foo"))
  (is (url-equal (merge-urls #u"/foo" :defaults #u"?foo=bar")
		 #u"/foo?foo=bar"))
  (is (url-equal (merge-urls #u"/foo" :defaults #u"#foo")
		 (make-url :program "foo"
			   :fragment "foo")))

  ;; Parameters against others
  ;; TODO: Write
  
  ;; Fragment against others
  ;; TODO: Write
  )