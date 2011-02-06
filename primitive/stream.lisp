

(defpackage :x  (:use :cl)
  (:export
    #:with-tee-to
    #:content-of
 ;;   #:make-relative-symbplic-link
    #:with-new-output-file
    #:make-relative-symbolic-link))

(in-package :x)

(import  (list 'excl.osi:with-open-temp-file 'excl.osi::make-temp-file-name 'excl:file-contents 
              'excl.osi::file-string-length 'excl::file-length 'excl::with-open-file))
(export  (list 'with-open-temp-file 'make-temp-file-name 'file-contents 'file-string-length
               'file-length 'with-open-file))

(defmacro with-tee-to (file &body body)
  (let ((stream (gensym "TEE")))
    `(with-open-file (,stream ,file :direction :output
                              :if-exists :supersede)
       (let ((*standard-output* (make-broadcast-stream *standard-output*
                                                       ,stream)))
         ,@body))))

(defun content-of (pathname)
  "Returns a string with the entire contents of the specified file."
  (with-output-to-string (contents)
    (with-open-file (in pathname :direction :input)
      (let* ((buffer-size 4096)
             (buffer (make-string buffer-size)))
        (loop for size = (read-sequence buffer in)
	   do (write-string buffer contents :start 0 :end size)
	   while (= size buffer-size))))))

(defun make-relative-symbolic-link (file points-to &aux prefix-length)
  (assert (stringp file))
  (assert (stringp points-to))
  (if (> (setq prefix-length (mismatch file points-to)) 1)
    (let* ((slash-loc (+ 1 (position #\/ file :from-end t :end prefix-length)))
	    (file (subseq file slash-loc))
	    (points-to (subseq points-to slash-loc))
	    (dir (list ':relative)))
      (dotimes (i (length (cdr (pathname-directory (pathname file)))))
	(push :back dir))
      (setq dir (nreverse dir))
      (setq dir (append dir
		  (cdr (pathname-directory
			 (pathname points-to)))))
      (let ((new-link
	      (namestring
		(make-pathname
		  :host (pathname-host file)
		  :device (pathname-device file)
		  :directory dir
		  :name (pathname-name file)
		  :type (pathname-type file)))))
	(delete-file file)
	(format t "~a => ~a~%" file new-link)
	(excl.osi:symlink  new-link :target file)))
    (warn "don't know how to handle this one: ~a ~a." file points-to)))

;(sys:with-command-line-arguments ("") (rest)
;  (when (/= 1 (length rest)) (error-die "Wrong number of arguments."))

#+nil(defun symbolic-link-p (pathspec)
  (if (eq (excl.osi:file-kind pathspec) :symbolic-link)
    (namestring (osicat:READ-LINK pathspec)) nil))

#+nil (defun relativize-links (dir) 
  (let ((directory (pathname-as-directory dir)))
    (osicat:mapdir 
     (lambda (file)
       (setq file (merge-pathnames file))
       (let ((l (symbolic-link-p file)))
	 (when l (make-relative-link l (namestring file)))))
     directory)))

(defmacro with-new-output-file ((stream file) &body body)
  "Like WITH-OPEN-FILE, but specialized for output to a file that must
not already exist."
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists :error)
     (let ((*print-case* :downcase))
       ,@body)))
