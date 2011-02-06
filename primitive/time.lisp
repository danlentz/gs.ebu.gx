;;;;: -*-    mode: lisp; syntax: common-lisp; coding: utf-8; base: 10;      -*-
;;;;:
;;;;: time.lisp
;;;;:    
;;;;:
;;;;: Copyright 2009 Dan Lentz, Lentz Intergalactic Softworks
;;;;: I don't think you fellows would do so much RAPING and PILLAGING if you
;;;;: played more PINBALL and watched CABLE TELEVISION!!
;;;;:
;;;;: Updated: 06:28:05 22 May 2009 by dan 
;;;;: Created: Dan Lentz <dan@lentz.com> 2009-05-22

(defpackage :x (:use :cl)
            (:export
              #:time-ago
              #:uptime))

(in-package :x)

(let ((start-time (get-universal-time)))
  (defun uptime (&optional suppress-output)
    "Displays start time and current uptime for this lisp process. Or not.
Returns uptime in seconds."
    (let ((uptime (- (get-universal-time) start-time))
           (days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

      (unless suppress-output
	;; Pretty print start time
	(multiple-value-bind
          (second minute hour date month year day daylight-p zone)
          (decode-universal-time start-time)
	  (flet ((tz-string (zone daylight-p)
		   (multiple-value-bind (hours mins)
                     (truncate (* zone 60) 60)
		     (when daylight-p
		       (setf hours (1- hours))) ; 1+ in the ISO world.
		     (let ((sign (if (minusp zone) ;ISO tz == -CL tz.
                                   #\+
                                   #\-)))
		       (format nil "~c~2,'0d~2,'0d" sign (abs hours) (abs mins))))))
	    (format t
              "~&Current image started: ~a, ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~a~%"
              (nth day days)
              year month date
              hour minute second
              (tz-string zone daylight-p))))

	;; Pretty print uptime
	(multiple-value-bind (days day-part)
          (truncate uptime 86400)
	  (multiple-value-bind (hours min-secs)
            (truncate day-part 3600)
	    (multiple-value-bind (minutes seconds)
              (truncate min-secs 60)
	      (format t "Uptime: ~d days ~2,'0d:~2,'0d:~2,'0d~%"
                days hours minutes seconds)))))
      uptime)))



(defun time-ago (utc)
  (let ((seconds (- (get-universal-time) utc)))
    (multiple-value-bind (minutes seconds) (floor (round seconds) 60)
      (multiple-value-bind (hours minutes) (floor minutes 60)
        (multiple-value-bind (days hours) (floor hours 24)
        (multiple-value-bind (weeks days) (floor days 7)
          (format nil  "~@{~[~*~:;~:*~:d ~(~a~2:*~p~*~)~^, ~]~}" 
                  weeks :week
                  days :day
                  hours :hour
                  minutes :minute
                  seconds :second)))))))

;; (defsuite time-suite)
;; (in-suite time-suite)

;; (deftest time-utils ()
;;   (is (string-equal "20 seconds" (let ((x (get-universal-time)))
;;                                    (sleep 20)
;;                                    (x:time-ago x)))))


;; (time-suite)
