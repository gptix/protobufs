;;; From _On Lisp_
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		      :from-end t
		      :initial-value (apply fn1 args))))
      #'identity))




;;; Functions for legibility. 
(defun not-last (byte)
  (<= 127 byte))

(defun deflag (byte)
  (mod byte 128))

(defun shift-left-7*n-bits (byte n)
  (ash byte (* 7 n)))

(defun field-num&wire-type (varint)
   (floor varint #b1000))

(defun field-type<-field-num (field-num)
  (getf *field-type-by-field-number* field-num))



;;; The algorithm.
(defun 7bit-words<-stream (strm)   
  (loop
     for byte = (read-byte strm nil)
     while (not-last byte)
     collect (deflag byte) into bag
     finally (return (reverse (cons byte bag)))))

(defun varint<-7bits (words)
  (apply #'+ (loop
		for w in words
		for i from 0
		collect (shift-left-7*n-bits w i))))

(setf (symbol-function 'varint<-stream)
      (compose #'varint<-7bits #'7bit-words<-stream))
	 
(defun key<-stream (strm)
  (multiple-value-bind
   (field-number wire-type)
   (field-num&wire-type (varint<-stream strm))
   `(:FIELD-NUMBER ,field-number :WIRE-TYPE ,wire-type)))
  
(defun kv<-stream (strm)
  (let ((k (key<-stream strm)))
    `(:KEY ,k :VALUE ,(case (getf k :WIRE-TYPE)
		      (0 (varint<-stream strm))
		      (t "unexpected wire-type")))))




;;; Test data and test function.
(with-open-file (strm "testdata"
		      :direction :output
		      :if-exists :supersede
		      :element-type '(unsigned-byte 8))
  (write-byte #x08 strm)
  (write-byte #x96 strm)
  (write-byte #x01 strm))

(defparameter *field-type-by-field-number*
  '(0 int64 1 int32))

(setf test-strm (open "testdata" 
		      :direction :input
		      :element-type '(unsigned-byte 8)))

(varint<-stream test-strm)

(defun zoo ()
  (with-open-file (strm "testdata"
			:direction :input
			:element-type '(unsigned-byte 8))
    (read-byte strm nil) ; discard first byte, since it just specifies wire type varint and field number 1.
    (varint<-stream strm)))
