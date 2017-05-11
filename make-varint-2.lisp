(defun key-value-pairs<-protobuf-stream (strm)
  "If the stream is not end-of-stream, passes the first byte read to a function to kv<-varint-and-stream."
(loop
;     for deadman from 1 to 10
     while (setf v (varint<-stream strm))
     collect (kv<-varint-and-stream v strm)))

(defun kv<-varint-and-stream (vrint strm)
  "Completes parsing of field key, and parses field.
Right now only works on int32 and int64 varints."
  (let ((fld-n (field-number<-varint vrint)))
    (list :FIELD-NUMBER
	  fld-n
	  :VALUE
	  (funcall (field-parser<-field-number fld-n) strm))))


;;; Local utilities
(defun not-last (byte)
  "True iff leftmost bit is 1."
  (<= 127 byte))

(defun deflag (byte)
  "Removes left bit."
  (mod byte 128))

(defun shift-left-7*n-bits (byte n)
  "Used to assemble a large number from 7-bit words."
  (ash byte (* 7 n)))


(defun 7bit-words<-stream (strm)
  "Reads bytes from a stream through an unflagged (flagged = left bit is 1) byte, then returns all bytes unflagged."
  (loop
     for byte = (read-byte strm nil)
     while byte
     while (not-last byte)
     collect (deflag byte) into bag
     finally (return (if byte
			 (reverse (cons byte bag))
			 bag))))

(defun varint<-7bits (words)
  "Calculates a number from 7-bit bytes received in little-endian order." 
  (if words
      (apply #'+ (loop
		    for w in words
		    for i from 0
		    collect (shift-left-7*n-bits w i)))))

(defun varint<-stream (strm)
  "Reads bytes from a stream and assembles them into a varint."
  (varint<-7bits (7bit-words<-stream strm)))

(defun field-number<-varint (vrint)
  "Chops off the three rightmost bits in a varint.  The remaining bits encode a protobuffer field number."
  (ash vrint -3))

(defun field-parser<-field-number (fn)
  (field-parser<-field-type (field-type<-field-number fn)))

(defun field-type<-field-number (fn)
  (getf '(0 int64
	  1 int32)
	fn))

(defun field-parser<-field-type (ft)
  (getf `(int32 varint<-stream
		int64 varint<-stream
		uint64 unknown
		uint32 unknown
		sint32 unknown
		sint64  unknown 
		bool  unknown 
		enum  unknown 
		fixed64  unknown 
		double  unknown 
		string  unknown 
		bytes  unknown 
		embedded-messages  unknown 
		packed-repeated-fields  unknown 
		fixed32  unknown 
		sfixed32  unknown 
		float  unknown)
	ft))




;;; The algorithm
;;; This retrieves one field.  I need to wrap this with a fn that will
;;; retrieve fields until a stream is consumed.

#|
(defun kv-pair<-stream (strm)
  (let ((fld-n (field-number<-varint (varint<-stream strm))))
    `(:FIELD-NUMBER
      ,fld-n
      :VALUE
      ,(funcall (field-parser<-field-number fld-n) strm))))

(defun protobuf-data<-stream (strm)
  (loop
     while (peek-char nil strm)
     collect (kv-pair<-stream strm)))


;;; Test data and test function.
(with-open-file (strm "testdata"
		      :direction :output
		      :if-exists :supersede
		      :element-type '(unsigned-byte 8))
  (write-byte #x08 strm)
  (write-byte #x96 strm)
  (write-byte #x01 strm))

#|

(setf test-strm (open "testdata" 
		      :direction :input
		      :element-type '(unsigned-byte 8)))

(varint<-stream test-strm)

|#
(defun field-num&wire-type (varint)
   (floor varint #b1000))
	 
