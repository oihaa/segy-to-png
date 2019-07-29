;;;; segy-to-png.lisp

(in-package #:segy-to-png)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defvar *cmap* (make-array 256))

(defun init-cmap (file)
  (csv-parser:do-csv-file ((fields num) file  :skip-lines 1)
    (let* ((i (parse-integer (car fields)))
	   (vals (loop for f in (cdr fields)
		    collect (parse-integer f))))
      (setf (aref *cmap* i) vals))))


(init-cmap  (merge-pathnames "cmap.csv"
                   (asdf:system-source-directory :segy-to-png)))

(defun stream-image-inline (out-stream filename lineno decimation)
  (segyio:with-inline (line-buffer filename lineno samples xlines decimation)
    (declare (type fixnum xlines decimation samples))
    (declare (type (simple-array single-float) line-buffer))
    (let* ((xlines (the fixnum (truncate (/ xlines decimation))))
	   (size (the fixnum (* samples  xlines))))
      (declare (type fixnum xlines size))
      (let* ((min-max (loop for i below size
			 for elm = (row-major-aref line-buffer i )
			 minimizing elm into min of-type single-float
			 maximizing elm into max of-type single-float
			      finally (return (list min max))))
	     (min-max (list -10.0f0 40.0f0))
	     (min-val (the single-float (first min-max)))
	     (max-val (the single-float (second min-max)))
	     (range (- max-val min-val))
	     (half-range (/ range 2.0f0))
	     (factor (the single-float (/ 255.0 range))))
	(macrolet ((scale (val)
		     `(the fixnum (floor (min (max (* (+ ,val half-range) factor) 0.0f0) 255.0f0)))))
	  (let ((png (make-instance 'zpng::pixel-streamed-png/palette
				    :color-type :indexed-color
				    :palette *cmap*
				    :width xlines
				    :height samples)))
	    (macrolet ((get-loc (i)
			 `(+  (* (mod ,i xlines) samples) (truncate (/ ,i xlines)))))
	      (zpng:start-png png out-stream)
	      (loop for i below samples
		 do (loop for j below xlines
		       ;;			 for p = (get-loc i)
		       do (zpng:write-pixel (list (scale (aref line-buffer i j )))  png)))
	      (zpng:finish-png png))))))))

(defun stream-image-xline (out-stream filename lineno decimation)
  (segyio:with-xline (line-buffer filename lineno samples inlines decimation)
    (let* ((inlines  (truncate (/ inlines decimation)))
	   (size (* samples inlines)))
      (let* ((min-max (loop for i below size
			 for elm = (autowrap:c-aref line-buffer i :float)
			 minimizing elm into min
			 maximizing elm into max
			 finally (return (list min max))))
	     (min-val (first min-max))
	     (max-val (second min-max))
	     (factor (/ 255.0 (- max-val min-val))))
	(flet ((scale (val)
		 (floor (* (- val min-val) factor))))
	  (let ((png (make-instance 'zpng::pixel-streamed-png/palette
				    :color-type :indexed-color
				    :palette *cmap*
				    :width inlines
				    :height samples)))
	    (flet ((get-loc (i)
		     (+  (* (mod i inlines) samples) (truncate (/ i inlines)))))
	      (zpng:start-png png out-stream)
	      (loop for i below size
		 for p = (get-loc i)
		 do (zpng:write-pixel (list (scale (aref line-buffer p )))  png))
	      (zpng:finish-png png))))))))     
