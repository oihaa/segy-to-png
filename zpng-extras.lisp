(in-package #:zpng)

(defclass pixel-streamed-png/palette (pixel-streamed-png)
  ((palette :initarg :palette :accessor palette)))

(defmethod write-png-stream ((png pixel-streamed-png/palette) stream)
  (check-size png)
  (write-png-header png stream)
  (write-ihdr png stream)
  (write-plte png stream)
  (write-idat png stream)
  (write-iend png stream))

(defmethod write-plte ((png pixel-streamed-png/palette) stream)
  (assert (eq (ihdr-color-type png)
	      (cdr (assoc :indexed-color *color-types*))))
  (assert (slot-boundp png 'palette))
  (let ((chunk (make-chunk 80 76 84 69 (* 256 3))))
    (loop for i below 256
	 for (r g b) across (palette png)
       do (progn
	    (chunk-write-byte r chunk)
	    (chunk-write-byte g chunk)
	    (chunk-write-byte b chunk)))
    (write-chunk chunk stream)))

(defmethod start-png ((png pixel-streamed-png/palette) stream)
  (call-next-method)
  (write-plte png stream)
  stream)
