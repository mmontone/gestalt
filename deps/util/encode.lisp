(in-package :gst.encode)

(defvar *key* nil
  "Key used for symmetric encryption (AES) of continuations
NIL -> no encryption")

(defun register-key (key)
  (setf *key*
        (let* ((octets (string-to-octets key))
               (length (length octets)))
          (and (> length 0)
               (flet ((pad-to (n)
                        "Is that a good idea, or should I just 0-pad?"
                        (apply 'concatenate
                               '(vector (unsigned-byte 8))
                               (subseq octets 0 (rem n length))
                               (make-list (floor n length)
                                          :initial-element octets))))
                 (ironclad:make-cipher :aes
                                       :key  (cond ((<= length 16) (pad-to 16))
                                                   ((<= length 24) (pad-to 23))
                                                   ((<= length 32) (pad-to 32))
                                                   (t (error "Maximum key size for AES: 32 bytes")))
                                       :mode :ecb))))))

(defun compress (string)
  (zlib:compress (string-to-octets string) :fixed))

(defun inflate (ub8s)
   (octets-to-string (uncompress ub8s)))

(defun word-to-octets (x)
  (declare (type (unsigned-byte 32) x))
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents (list (ldb (byte 8 0) x)
                                      (ldb (byte 8 8) x)
                                      (ldb (byte 8 16) x)
                                      (ldb (byte 8 24) x))))

(defun octets-to-word (octets)
  (+ (* (expt 2 0)  (aref octets 0))
     (* (expt 2 8)  (aref octets 1))
     (* (expt 2 16) (aref octets 2))
     (* (expt 2 24) (aref octets 3))))

(defun encrypt (octets)
  (if (null *key*)
      octets
      (let* ((octets (concatenate '(vector (unsigned-byte 8))
                                  (word-to-octets (length octets))
                                  octets))
             (octets (concatenate '(vector (unsigned-byte 8))
                                  octets
                                  (make-array (- (* 16 (ceiling (length octets) 16))
                                                 (length octets))
                                              :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
             (out    (make-array (* 2 (length octets))
                                 :element-type '(unsigned-byte 8)))
             (length (nth-value 1
                                (ironclad:encrypt *key*
                                                  octets
                                                  out))))
        (subseq out 0 length))))

(defun decrypt (octets)
  (if (null *key*)
      octets
      (let* ((out (make-array (* 2 (length octets))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0))
             (length (nth-value 1
                                (ironclad:decrypt *key* octets out)))
             (out    (subseq out 0 length)))
        (subseq out 4 (+ 4 (octets-to-word out))))))

(defclass object ()
  ())

#|

(defun encryption-example ()
  (register-key "mmontone")
  (let* ((objects (make-hash-table))
	 (my-object (make-instance 'object))
	 (address (sb-kernel:get-lisp-obj-address my-object))
	 (address-string (format nil "~A" address)))
    ;; Keep the object
    (setf (gethash address objects) my-object)
    ;; Encode the address
    (format t "The object is: ~A~%" my-object)
    (let* ((encoded-address (cl-base64:usb8-array-to-base64-string (encrypt (sb-ext:string-to-octets address-string)) :uri t)) ;; note the :uri parameter!!!
	   (decoded-address (sb-ext:octets-to-string (decrypt (cl-base64:base64-string-to-usb8-array encoded-address :uri t)))))
      (format t "The encoded address is: ~A~%" encoded-address)
      (format t "The URL would be: ~A~%" (url-encode encoded-address))
      (format t "The decoded address is: ~A~%" decoded-address)
      (format t "The recovered object is: ~A~%" (gethash (parse-integer decoded-address) objects)))))

|#