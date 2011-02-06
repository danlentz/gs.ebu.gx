;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-log/cl-log.lisp#15 $
;;
;; Based on CL-LOG.LISP by Nick Levine, Ravenbrook Limited, 2007-05-15
;;

(defpackage :x (:use :cl)
            (:export :when-let :orf :ring :make-ring :ring-push :ring-list
                     :log-manager :log-message :log-object :with-logging-disabled
                     :log-manager-messengers :log-manager-message-class :rebinding-log-manager
                     :invalidate-log-manager :start-messenger :ring-messenger
                     :ring-messenger-messeges :base-message :formatted-message :format-message
                     :message-description :message-text :message-time-index :message-category
                     :message-arguments :time-index :make-time-index :time-index-fraction
                     :time-index-universal-time :messenger-category :base-messenger
                     :start-messenger :stop-messenger :find-messenger :messenger-send-message
                     :text-stream-messenger-stream :text-stream-messenger :text-file-messenger
                     :text-file-messenger-file :clear-categories :defcategory :undefcategory
                     :defcategory-fn :category-satisfies))

(in-package :x)

(defmacro orf (location form &environment env)
  (multiple-value-bind (vars values new setter getter)
      (get-setf-expansion location env)
    (when (cdr new)
      (error "Can't work with setf-expansion for ~s - ~d values from setter ~s"
             location (length new) new))
    (let ((current (car new)))
      `(let* (,@(mapcar 'list vars values)
              (,current ,getter))
         (or ,current
             (progn (setf ,current ,form)
               ,setter))))))

(defmacro when-let (binding &body body)
  (destructuring-bind (var val) binding
    `(let ((,var ,val))
       (when ,var
	 ,@body))))

#+not-in-use
(defmacro when-let* (bindings &body body)
  (if bindings
      `(when-let ,(car bindings)
	 (when-let* ,(cdr bindings)
	   ,@body))
    `(progn ,@body)))


;; 3.  LOG-MANAGER

(defvar *log-manager* nil)

(defclass log-object ()
  ())

(defclass log-manager (log-object)
  ((messengers      :accessor log-manager-messengers    :initform nil)
   (disabled        :accessor logging-disabled-var      :initform (gensym))
   (message-class   :accessor log-manager-message-class :initarg :message-class)
   (message-id      :accessor log-manager-id            :initform 0)   ;  id of latest message
   (category-cache  :reader   category-cache            :initform nil)
   (cache-version   :accessor cache-version)
   (first-time      :reader   log-manager-first-time    :initform (first-time-for-log-manager))))

(defmethod initialize-instance :after ((self log-manager) &key disabled)
  (setf (logging-disabled self) disabled)
  (invalidate-log-manager self))

(defun first-time-for-log-manager ()
  (- (* (get-universal-time)
        internal-time-units-per-second)
     (get-internal-real-time)))

(defmethod logging-disabled ((self log-manager))
  (symbol-value (logging-disabled-var self)))

(defmethod (setf logging-disabled) (new-value (self log-manager))
  (setf (symbol-value (logging-disabled-var self)) new-value))

(defmacro with-logging-disabled (&body body)
  `(progv `(,(logging-disabled-var (log-manager))) '(t)
     ,@body))

(defmethod (setf log-manager-messengers) :after (new-value (self log-manager))
  (declare (ignore new-value))
  (invalidate-log-manager self))

(defmethod category-cache :before ((self log-manager))
  (when (< (cache-version self) (category-version))
    (invalidate-log-manager self)))

(defmethod invalidate-log-manager ((self log-manager))
  (let ((cache (orf (slot-value self 'category-cache)
                    (make-hash-table :test 'equal))))
    (clrhash cache)
    (setf (cache-version self) (category-version))))

(defun log-manager ()
  *log-manager*)

(defun (setf log-manager) (log-manager)
  (unless (typep log-manager '(or log-manager null))
    (error "New log-manager is neither null nor a log-manager: ~s" log-manager))
  (when-let (previous (log-manager))
    (dolist (messenger (log-manager-messengers previous))
      (stop-messenger messenger)))
  (setf *log-manager* log-manager))

(defmacro rebinding-log-manager ((log-manager) &body body)
  (let ((log-manager-var (gensym "LOG-MANAGER-")))
    `(let ((,log-manager-var ,log-manager))
       (unless (typep ,log-manager-var '(or log-manager null))
         (error "New log-manager is neither null nor a log-manager: ~s" ,log-manager-var))
       (setf (slot-value ,log-manager-var 'category-cache) nil)
       (invalidate-log-manager ,log-manager-var)
       (let ((*log-manager* ,log-manager-var))
         ,@body))))


;; 4.  MESSAGE

;; Warning: the fraction will be self-consistent but not externally consistent: the fraction
;; won't be zero when the univeral-time changes. (If we wanted this we'd have to wait for it,
;; and we still might not get to it spot-on.)

(defstruct (time-index
             (:constructor construct-time-index (universal-time fraction)))
  (universal-time nil :read-only t)
  (fraction       nil :read-only t))

(defun make-time-index (log-manager)
  (let* ((first-time (log-manager-first-time log-manager))
         (this-time (+ first-time (get-internal-real-time))))
    (multiple-value-bind (univeral-time fraction)
        (floor this-time internal-time-units-per-second)
      (construct-time-index univeral-time fraction))))

(defmethod print-object ((self time-index) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :type t :identity t)
        (let ((*print-escape* nil))
          (print-object self stream)))
    (format stream #.(format nil "~~d.~~~d,'0d" (ceiling (log internal-time-units-per-second 10)))
            (time-index-universal-time self)
            (time-index-fraction self))))

(defclass base-message (log-object)
  ((id          :reader message-id          :initform (incf (log-manager-id (log-manager))))
   (time-index   :reader message-time-index   :initarg :time-index)
   (category    :reader message-category    :initarg :category)
   (description :reader message-description :initarg :description)
   (arguments   :reader message-arguments   :initarg :arguments)))

(defmethod print-object ((self base-message) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d" (message-id self))))

(defmethod initialize-instance :after ((self base-message) &key time-index)
  (unless time-index
    (error "Message with no time-index: ~s" self)))

(defclass formatted-message (base-message)
 ((text :accessor formatted-message-text :initform nil)))

(defmethod message-text ((self formatted-message))
  (orf (formatted-message-text self)
       (format-message self)))

(defmethod format-message ((self formatted-message))
  (format nil "~a ~a ~?~&"
          (message-time-index self)
          (message-category self)
          (message-description self)
          (message-arguments self)))


;; 5.  MESSENGER

(defclass base-messenger (log-object)
  ((name     :reader messenger-name     :initarg :name     :initform nil)
   (category :reader messenger-category :initarg :category :initform nil)))

(defmethod print-object ((self base-messenger) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (when-let (name (messenger-name self))
      (format stream "~a" name))))

(defmethod initialize-instance :after ((self base-messenger) &key name)
  (when (typep name 'base-messenger)
    (error "It really doesn't help using one messenger ~s to name another ~s" name self)))

(defun start-messenger (class &rest initargs &key name &allow-other-keys)
  (when-let (previous (find-messenger name))
    (stop-messenger previous))
  (let ((messenger (apply 'make-instance class initargs)))
    (push messenger (log-manager-messengers (log-manager)))
    messenger))

(defmethod stop-messenger ((self base-messenger))
  (let ((messengers (log-manager-messengers (log-manager))))
    (when (find self messengers)
      (setf (log-manager-messengers (log-manager))
            (remove self messengers)))))

(defmethod stop-messenger (name)
  (let ((messenger (find-messenger name)))
    (if messenger
        (stop-messenger messenger)
      (error "Messenger named ~s not found" name))))

(defun find-messenger (name)
  (find name (log-manager-messengers (log-manager))
        :key 'messenger-name
        :test 'equalp))

(defun category-messengers (category)
  (let* ((manager (or (log-manager)
                      ;; we're not logging right now, so nothing to do
                      (return-from category-messengers nil)))
         (cache (category-cache manager)))
    (unless (logging-disabled manager)
      (multiple-value-bind (satisfies presentp)
          (gethash category cache)
        (if presentp
            satisfies
          (setf (gethash category cache)
                (loop for messenger in (log-manager-messengers manager)
                      when (category-satisfies category (messenger-category messenger))
                      collect messenger)))))))

;; Have we satisfied the requested category?
;; The requested category is either a keyword or a logical combination
;; of keywords held together with AND, OR and NOT.
;; The supplied category is either a keyword or a list of keywords in
;; which case the implicit combination is AND.
;; [I am unconvinced that there's anything other than unnecessary complexity
;;  to be gained from category being more general than this.]
;; [Although the code doesn't enforce keywords, I am suggesting this to allow for future
;; expansion, e.g. supplying funcallables.]
;; (category-satisfies '(:this :that) '(or :this :that)) => T
;;  needed either, got both, so satisfied

(defun category-satisfies (supplied requested)
  (unless (listp supplied)
    (setf supplied (list supplied)))
  (in-category-satisfies supplied requested supplied))

(defun in-category-satisfies (supplied requested expanded)
  (typecase requested
    (null t)
    (atom (let ((expansion (unless (find requested expanded)
                             (expand-category requested))))
            (if expansion
                (in-category-satisfies supplied expansion (cons requested expanded))
              (not (null (find requested supplied))))))
    (t (ecase (car requested)
         ((and) (every (lambda (r) (in-category-satisfies supplied r expanded)) (cdr requested)))
         ((or)  (some  (lambda (r) (in-category-satisfies supplied r expanded)) (cdr requested)))
         ((not) (if (cddr requested)
                    (error "(Sub)category NOT with more than one 'argument': ~s" requested)
                  (not (in-category-satisfies supplied (cadr requested) expanded))))))))

(defun send-message (messengers category description arguments)
  (let* ((log-manager (log-manager))
         (message (make-instance (log-manager-message-class log-manager)
                                 :time-index (make-time-index log-manager)
                                 :category category
                                 :description description
                                 :arguments arguments)))
    (loop for messenger in messengers do
          (messenger-send-message messenger message))))

(defmethod messenger-send-message ((messenger base-messenger) message)
  (error "Messenger ~s of class ~s has not specialised ~s for message ~s of class ~s"
         messenger
         (class-of messenger)
         'messenger-send-message
         message
         (class-of message)))


;; 5.1. Ring-Messenger
;;
;; A simple example messenger. We define a ring structure and a class
;; ring-messenger which will remember the last N log-messages cheaply.
;; We have specialised messenger-send-message as required.  We have
;; not specialised stop-messenger as doing so is optional and in this
;; case there's nothing to do.

(defstruct (ring
             (:constructor construct-ring (name ring length)))
  name
  ring
  length)

(defmethod print-object ((self ring) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~(~a~) (~d)"
            (ring-name self)
            (ring-length self))))

(defun make-ring (name length)
  (let ((ring (make-list length)))
    (setf (cdr (last ring)) ring)
    (construct-ring name ring length)))

(defun ring-push (thing ring)
  (setf (car (setf (ring-ring ring)
                   (cdr (ring-ring ring))))
        thing))

(defun ring-list (ring)
  (loop repeat (ring-length ring)
        for x in (cdr (ring-ring ring))
        collect x))

(defclass ring-messenger (base-messenger)
  ((ring :reader ring-messenger-ring)))

(defmethod initialize-instance :after ((self ring-messenger) &key name length)
  (setf (slot-value self 'ring) (make-ring name length)))

(defmethod print-object ((self ring-messenger) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~(~a~)" (ring-name (ring-messenger-ring self)))))

(defmethod messenger-send-message ((messenger ring-messenger) (message base-message))
  (ring-push message (ring-messenger-ring messenger)))

(defmethod ring-messenger-messages ((self ring-messenger))
  (remove nil (ring-list (ring-messenger-ring self))))


;; 5.2. Text-Stream-Messenger

(defclass text-stream-messenger (base-messenger)
  ((stream :reader text-stream-messenger-stream :initarg :stream)
   (closed :accessor text-stream-messenger-closed :initform nil)))

(defmethod messenger-send-message ((messenger text-stream-messenger) (message formatted-message))
  (let ((ostream (text-stream-messenger-stream messenger)))
    (handler-bind
        (;; Trap race condition where thread A starts a logging operation and lists this as one of
         ;; its messengers, thread B stops the messenger, and then thread A attempts to complete its
         ;; logging operation by writing to ostream (now closed). The alternatives would be to halt
         ;; preemption (application-specific and maybe costly) or to handle all logging operations 
         ;; in a dedicated thread (also application-specific and maybe costly).
         (serious-condition (lambda (condition)
                              (declare (ignore condition))
                              (when (text-stream-messenger-closed messenger)
                                (return-from messenger-send-message)))))
      (write-string (message-text message) ostream))
    (ignore-errors
      (force-output ostream))))

(defmethod stop-messenger :before ((self text-stream-messenger))
  (let ((stream (text-stream-messenger-stream self)))
    (setf (text-stream-messenger-closed self) t)
    (ignore-errors
      (force-output stream))
    (close stream)))


;; 5.3 Text-File-Messenger

(defclass text-file-messenger (text-stream-messenger)
  ((file :reader text-file-messenger-file :initarg :filename)))

(defmethod initialize-instance :after ((self text-file-messenger) &key filename (external-format :default) &allow-other-keys)
  (setf (slot-value self 'stream)
        (open filename
              :direction :output
              :element-type :default
              :if-does-not-exist :create
              :if-exists :append
              :external-format external-format)))


;; 6.  CATEGORY

(defvar *categories* (make-hash-table :test 'eq))
(defvar *category-version* 0)
(defun category-version () *category-version*)

(defun expand-category (category)
  (or (gethash category *categories*)
      category))

;; (defcategory :debug (or :debug :info)) will work.
;; Note that (defcategory :critical) doesn't have any effect other than to make your code clearer.
(defmacro defcategory (category &optional expands-as)
  `(defcategory-fn ',category ',expands-as))

(defun defcategory-fn (category expands-as)
  (setf (gethash category *categories*) expands-as)
  (incf *category-version*)
  category)

(defmacro undefcategory (category)
  `(undefcategory-fn ',category))

(defun undefcategory-fn (category)
  (remhash category *categories*)
  (incf *category-version*)
  nil)

(defun clear-categories ()
  (clrhash *categories*)
  (incf *category-version*)
  nil)


;; 7.  LOG-MESSAGE

;; By making this a macro we can defer evaluation of description and arguments until we know
;; that the message will be sent somewhere. The idea is to make :wombat logging very cheap when
;; :wombat logging isn't enabled

(defmacro log-message (category description &rest arguments)
  (if (member :no-logging *features*)
      `(values)
    (let ((category-var (gensym "CATEGORY-"))
          (messengers-var (gensym "MESSENGERS-")))
      `(let ((,category-var ,category))
         (when-let (,messengers-var (category-messengers ,category-var))
                                        ; null when logging-disabled is set
           (send-message ,messengers-var ,category-var
                         ,description (list ,@arguments)))
         nil))))

(defcategory :critical)
(defcategory :error   (or :error :critical))
(defcategory :warning (or :warning :error))
(defcategory :notice  (or :notice :warning))
(defcategory :info    (or :info :notice))
(defcategory :debug   (or :debug :info))

(assert (setf (log-manager) (make-instance 'log-manager :message-class 'base-message)))
(assert (start-messenger 'ring-messenger :name 'info-ring :length 24 
                         :category '(and :info (not :error))))

;; Show that we logged everything which was at least :info provided it wasn't also at least :error
(assert (not (set-difference 
              (dolist (z '(:critical :error :warning :notice :info :debug))
                (log-message z z))
              (loop
                 for item in (ring-messenger-messages (car (log-manager-messengers (log-manager))))
                 when item collect (message-description item)))))


;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;; 2007-05-15 Created.
;;
;;
;; C.  COPYRIGHT
;;
;; This file copyright (c) 2007 - 2009 Nick Levine (ndl@ravenbrook.com)
;; Log5 copyright (c) 2007 Gary Warren King (gwking@metabang.com)

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

