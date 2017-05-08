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




;;; The algorithm.
(defun 7bit-words (strm)   
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
      (compose #'varint<-7bits #'7bit-words))
	 



;;; Test data and test function.
(with-open-file (strm "testdata"
		      :direction :output
		      :if-exists :supersede
		      :element-type '(unsigned-byte 8))
  (write-byte #x08 strm)
  (write-byte #x96 strm)
  (write-byte #x01 strm))


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

     
       
       

;;;; Assumption: we have read a byte that indicates that the folowing string (unknown length) of bytes encodes a varint.

;;;; Step 1: read bytes until the final byte is found.
;;;; For each byte, push the right seven bits onto a bit stack.
;;;; 


(defun make-bit-vector ()
  (make-array 0
	      :element-type 'bit
	      :fill-pointer 0
	      :adjustable t))

;;;;(defun get-varint (streem)
;;;;  (* 2 (reverse (accumulate-bits streem (make-bit-vector)))))

;;;;(defun more-bytes-after (this-byte)
;;;;  (= 1 (floor this-byte 128)))

;;;;(defun value-bits (this-byte)
;;;;  (right7 this-byte))

;;;;;;;;;;;;;;;;;;

(defun read-protobuf-message (strm)
  "Read a protocol buffer message from a stream."
  (multiple-value-bind
	(type wire)
      (floor (read-varint strm 0) 8)
    (case wire
      (0 `(:TYPE ,type :VALUE ,(read-varint strm 0)))
      (t "Error.  Unexpected wire type \"~a\" found." wire))))

(defun read-varint (strm so-far)
  "Read a protocol buffer variable-length integer from a stream."
  (multiple-value-bind
	(more-bits val-bits) ; MSB is a flag (1 = not last byte)
      (floor (read-byte strm) 128) ; right 7 bits are part of value.
    (if (= 1 more-bits)
	(read-varint strm (+ (ash so-far 7) val-bits))
	(+ (ash so-far 7) val-bits))))

(defun read-protobuf-message-from-file (filename)
  (with-open-file (strm filename 
			:direction :input
			:element-type 'unsigned-byte)
    (read-varint strm 0)))

;;;;;;;;;;;;;;;;;;


(defun encode-zigzag (num)
  (if (minusp num)
      (- (ash (abs num) 1) 1)
      (ash num 1)))


(defun decode-zigzag (znum)
  (if (evenp znum)
      (ash znum -1) ; znum encodes a positive num
      (* -1 (1+ (ash znum -1))))) ; znum encodes a negative num


	  (format t   "~a~%"  `(:MORE-BITS ,more-bits :VAL-BITS ,val-bits)))
	(progn
	  (format t   "No more bits: ~a~%"  `(:MORE-BITS ,more-bits :VAL-BITS ,val-bits))
	  (format t "Varint value: ~a~%" (+ (ash so-far 7) val-bits))))
))
    
    	;; Recurse, passing sum of so-far (<<<7) and val-bits.
	;; Return sum of so-far (<<<7) and val-bits.
	

(defun foo1 (byte)
  (multiple-value-bind
	(more-bits val-bits) ; MSB is a flag (1 = not last byte)
      (floor byte 128) ; right 7 bits are part of value.
    (+ (ash 0 7) val-bits)))
    
    `(:MORE-BITS ,more-bits :VAL-BITS ,val-bits)))


;; Recurse, passing sum of so-far (<<<7) and val-bits.
(process-varint streem (+ (ash so-far 7) val-bits))
;; Return sum of so-far (<<<7) and val-bits.
(+ (ash so-far 7) val-bits))))

;;;;;;;;;;;;;;;;;;;;

#|

(with-open-stream (*standard-output* "test-varint"
				   :direction :output
				   :if-does-not-exist :create) 	   
  (print test-vrint *standard-output* ))


(with-input-from-stream (streem test-vrint :element-type '(UNSIGNED-BYTE 16))
  (read-byte streem))


|#

;;; Process one byte by shifting the result so far left by 7 bits, then,
;;; if the current byte has a flag that in
(defun  process-varint (streem so-far)
  "Process a stream feeding an unknown number of bytes encoding a variable-length integer."
  (multiple-value-bind
	(more-bits val-bits) ; MSB is a flag (1 = not last byte)
      (floor (read-byte streem) 128) ; right 7 bits are part of value.
    (if more-bits
	;; Recurse, passing sum of so-far (<<<7) and val-bits.
	(process-varint streem (+ (ash so-far 7) val-bits))
	;; Return sum of so-far (<<<7) and val-bits.
	(+ (ash so-far 7) val-bits))))



(defun num<-bit-vector (bv)
  (reduce (lambda (sum next-bit) (+ (ash sum 1) next-bit)) bv))

;;;; Per google page
;;;; https://developers.google.com/protocol-buffers/docs/encoding
;;;; 96 01 = 1001 0110  0000 0001

(format t "~a"  #b1001011000000001)
(format t "~a"  #b10010110)
(format t "~a"  #b0010110)
(format t "~a"  #b00101100000001)
(format t "~a~a ~a~a"  #b1001 #b0110 #b0000 #b0001)

(defun concat-lst-strings (strs)
  (format nil "~{~a~}" strs))

(defun parse-bin (bit-string)
  (parse-integer bit-string :radix 2))

(defun split-string (str first-after)
  `(,(subseq str 0 first-after) ,(subseq str first-after)))

(defun bcds (digits) ; as string
  (concat-lst-strings (mapcar #'bcd (concatenate 'list digits))))

(defun bcd (digit) ; as char
  (format nil "~4,'0b" (parse-integer (string digit))))
  
(defun line&wire (bits)
  `(,@(split-string bits 5)))

(defun flag&val-bits (bits)
  `(,@(split-string bits 1)))

(defun val-strings (strm)
  (destructuring-bind (flag val-bits)
      (flag&val-bits (bcds (format nil "~2,'0d" (read strm))))
;   `(,flag ,val-bits)))
    (if (string= "0" flag)
	`(,val-bits)
	(append `(,val-bits) (val-strings strm)))))

(defun value<-stream (strm)
  (parse-bin (concat-lst-strings (reverse (val-strings strm)))))

#|A varint line in a protobuf message will consist of two or more two-digit strings, separated by spaces.

The first numeral, when evaluated as a number and then examined as an 8-bit number, indicates, the line type number within a message type definition, and the wire type "varint".  The leftmost five bits evaluate to a number between 0 and 31, and this is the line type number.  The rightmost three bits evaluate to a numbe between 0 and 7, and indicate the wire type (0 for varint).

Subsequent two-digit strings are evaluated as follows:
1.  Split the two digit string into two numerals.

For each:
2.  Convert each to a four-character binary-coded decimal string.
3.  Concatenate these.
4.  Examine the leftmost bit.
5.  If that is a 1, append the seven rightmost bits as a string to a list that is the result of recursively evaluating the next two-numeral string in the message.
6.  If that is a 0, return a list with one element, to wit the seven rightmost bits as a string.
7.  Once all seven-bit strings have been collected, reverse the list of the 7-digit strings (NOT reversing the strings themselves).
8.  Concatenate all of the seven-digit strings into one string.
9.  Parse the remaining number as a binary number.

(
\
single element 
concatenate the seven rightmost bits with the result of recursively 

A varint value consists of a series of two-digit strings, separated by spaces.

To decode each string:

If the byte is the first, convert the string to a number, then examine it as a binary number.  The rightmost three bits indicate the wire type, which is 0 for a varint.  The leftmost five digits are the number of the line type definition in a message type definition.

For example:

The string "08" is converted to 8, or #b00001000, or #b00001 and #b000,
so the wire type is #b000 = 0, and the number of the line definition = 1.

For each following string, convert each digit to a four-bit binary-coded decimal number, then concatenating the two.

For example:

The string "96" converts to #b1001 and #b0110 -> #b10010110.

Split the resulting 8-bit word into a 1-bit flag (the leftmost bit) and a 7-bit value (the rightmost 7 bits).

If the flag is 1, concatenate the value, as binary digits, with the result of evaluating the next two-digit numeric string.

If the flag is zero, return the value as bits.




1.  Pa
a byte, the first five bits of which comprise the number of a message line definition, and the last three bits of which comprise the wire number specifying the encoding of the remaining bits (for varint, "000").

One or more bytes, each of which consist of two binary-coded-decimal four-bit nybbles.
To decode a byte:
Split 





(setf test- data "1010110000000010")

|#
#|
To understand your simple protocol buffer encoding, you first need to understand varints. Varints are a method of serializing integers using one or more bytes. Smaller numbers take a smaller number of bytes.

Each byte in a varint, except the last byte, has the most significant bit (msb) set – this indicates that there are further bytes to come. The lower 7 bits of each byte are used to store the two's complement representation of the number in groups of 7 bits, least significant group first.

So, for example, here is the number 1 – it's a single byte, so the msb is not set:

0000 0001
And here is 300 – this is a bit more complicated:

1010 1100 0000 0010

|#


(let ((strm (make-string-input-stream "Hello my little chickadee!")))
  (print (read-char strm))
  (print (read-char strm)))


(defun field-wire<-byte (field-wire)
  (multiple-value-bind (field-number wire-type)
      (floor (parse-integer field-wire) 8)
    `(,field-number ,wire-type)))


(defun flag-7bits<-byte (next-byte)
  (multiple-value-bind (flag 7bits)
      (floor next-byte 128)
    `(,flag ,7bits)))

(defun parse-varint (strng-strm)
  "Parses a series of bytes as a base-127 variable-length integer, encoded:
First bit = 0 -> this is the last byte, and the value is encoded in the remaining 7 bits of this bytegb
First bit = 1 -> this is not the finaly byte, and the value is encoded in the seven remaining bits of this byte, and the result of evaluating the next byte."
    (destructuring-bind (flag 7bits)
	(flag-7bits<-byte (read-char strng-strm nil nil)) 
      (if (zerop flag)
	  7bits)))


	  (format nil "~b" 7bits))))

	(concatenate 'string (format nil "~b" 7bits) (parse-varint strng-strm)))))

(defun test-parser ()
  (let ((a-stream (make-string-input-stream "96 01")))
    (parse-varint a-stream)))

    
    (mapcar (lambda (x)
	  (subseq x 0 7))
	(group (concatenate 'list tdata) 8))


(defun chars->string (chars)
  (let ((str (make-array 0
			 :element-type 'character
			 :fill-pointer 0
			 :adjustable t)))
    (dolist (char chars) (vector-push-extend char str))
  str))

(defun longer (x y)
  (labels ((compare (x y)
	     (and (consp x)
		  (or (null y)
		      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun make-varint (num)
  (loop for sett in (group
		     (concatenate 'list (reverse (format nil "~b" num)))
		     7)
       collecting (pad sett #\0 7)))

(defun foo (num)
  (let ((bytes (reverse
		(mapcar #'pad-left-with-zeros 
			(group-in-7-tuples-from-right
			 (dec-to-bin-chars num))))))
    (setf (caar bytes) #\0)
    bytes))
    

(defun pad (lst el lngth)
  (subseq (append lst (make-list lngth :initial-element el))
	  0 lngth))

(defun pad-and-tag (lst)
   (cons #\1 (reverse
    (subseq
     (reverse
      (append
       (make-list 7 :initial-element #\0) lst))
     0 7))))


(defun dec-to-bin-chars (num)
  (concatenate 'list (format nil "~b" num)))

(defun 7-tuples-right (lst)
  (mapcar (lambda (x)
	    (pad-and-tag (reverse x)))
	  (reverse (group (reverse lst) 7))))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

#|
1010 1100 0000 0010
|#
