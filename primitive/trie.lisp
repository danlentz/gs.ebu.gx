

(cl:defpackage :x
  (:use :cl)
  (:export :trie :navigate-trie :get-trie :lookup-prefix-in-trie :bulk-insert-trie))

(cl:in-package :x)

#|

This module implements tries. A trie is a recursive data structure that maps
lists of keys into values. Common key prefixes are shared. For example,
given the following mappings:

  a => 1, aa => 2, ab => 3, ba => 4,

we get the following trie (nodes are represented as [ value | branches...]):

                   +--[ 2 | nil ]
                  /
         +--[ 1 | a  b ]
        /            \
[ nil | a  b ]        +--[ 3 | nil ]
           \
            +--[ nil | a ]
                       \
                        +--[ 4 | nil ]

Note that nil and b don't have mappings in the above trie.

This implementation uses hash tables to store the mapping from branch keys
into branch nodes.

|#


(defclass trie ()
  ((value :initform nil
	  :accessor trie-value)
   (branches :initform (make-hash-table)
	     :accessor trie-branches))
  (:documentation "Represents a trie node. This module implements tries.
A trie is a recursive data structure that maps
lists of keys into values. Common key prefixes are shared. For example,
given the following mappings:  a => 1, aa => 2, ab => 3, ba => 4,
we get the following trie (nodes are represented as [ value | branches...]):

                   +--[ 2 | nil ]
                  /
         +--[ 1 | a  b ]
        /            \
[ nil | a  b ]        +--[ 3 | nil ]
           \
            +--[ nil | a ]
                       \
                        +--[ 4 | nil ]

Note that nil and b don't have mappings in the above trie.
This implementation uses hash tables to store the mapping from branch keys
into branch nodes."))

;;(export 'trie)


(defun navigate-trie (trie key-list &key create-new-branches)
  "Recursively navigates the trie, possibly creating necessary branches.
   Returns the final node or nil if not found and create-new-branches is nil.
   The second returned value is the value of the last node encountered."
  (with-slots (value branches) trie
    (if (null key-list)
        (values trie value) ; No more keys to process--return the current node.
        (destructuring-bind (next-key &rest remaining-keys) key-list
          (progn
            (unless (gethash next-key branches)
              ;; Not found. Create a new branch or fail.
              (if create-new-branches
                  (setf (gethash next-key branches) (make-instance 'trie))
                  (return-from navigate-trie (values nil value))))
            ;; Descend down the tree.
            (navigate-trie (gethash next-key branches)
                           remaining-keys
                           :create-new-branches create-new-branches))))))


(defgeneric get-trie (key-list trie)
  (:method ((key-list list) (trie trie))
    (awhen (navigate-trie trie key-list)
      (trie-value it))))

;;(export 'get-trie)


(defgeneric lookup-prefix-in-trie (key-list trie)
  (:documentation "Looks up the longest defined prefix of the supplied key in the trie.")
  (:method ((key-list list) (trie trie))
    (multiple-value-bind (_ value)
        (navigate-trie trie key-list)
      (declare (ignore _))
      value)))

;;(export 'lookup-prefix-in-trie)


(defgeneric  (setf get-trie) (val key-list trie)
    (:method (val (key-list list) (trie trie))
      (with-slots (value) (navigate-trie trie key-list :create-new-branches t)
        (setf value val))))


(defgeneric bulk-insert-trie (key-list trie &key map-fn)
  (:documentation "Inserts multiple items into the trie.
   The input list contains (key-list value) pairs.
   The optional map-fn can be supplied to map input list items.")
  (:method ((key-value-list list) (trie trie) &key map-fn)
    (dolist (key-value key-value-list)
      (destructuring-bind (key-list value)
          (if map-fn
              (funcall map-fn key-value)
              key-value)
        (setf (get-trie key-list trie) value)))
    trie))

;;(export 'bulk-insert-trie)

(defun sort-trie (trie predicate &rest args &key (key #'key) (stable nil) &allow-other-keys)
  "Sort a trie recursively with a predicate function."
  (let ((root trie))
    (sort-trie-branch root predicate :key key :stable stable)
    (unless (leafp trie)
      (loop as branch in (branches root) do
        (setf branch
              (sort-trie branch predicate :allow-other-keys t args))))
    root))

(defun sort-trie-branch (trie predicate &key (key #'key) (stable nil))
  "Sort a trie node’s branches with a predicate function."
  (let ((branches (copy-list (branches trie)))
        (sort (if stable #'stable-sort #'sort)))
    (setf (branches trie) (funcall sort branches predicate :key key))
    trie))
#+nil
(test "trie"
      '((nil 1 2 3 nil nil nil 4 nil)
        (nil nil 1 2 2))
      (let ((trie (make-instance 'trie)))
        (bulk-insert-trie '(((a) 1)
                            ((a a) 2)
                            ((a b) 3)
                            ((b a) 4))
                          trie)
        (list (mapcar #'(lambda (key-list) (get-trie key-list trie))
                      '(nil (a) (a a) (a b) (a c) (a c d) (b) (b a) (b a e)))
              (mapcar #'(lambda (key-list) (lookup-prefix-in-trie key-list trie))
                      '(nil (z) (a) (a a) (a a a))))))
