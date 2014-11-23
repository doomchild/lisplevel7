(in-package #:lisplevel7)

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Backspace #\Linefeed #\Page #\Return #\Rubout))

(defclass HL7Message ()
  ((value :reader value :initarg :text :initform nil)
   (delimiters :reader delimiters :initarg :delimiters)
   (segments :reader segments)))

(defmethod initialize-instance :around ((h HL7Message) &key text)
  (let* ((stripped (string-trim *whitespace* (remove-if-not #'standard-char-p text)))
	 (delimiters (get-delimiters stripped)))
    (call-next-method h :text stripped :delimiters delimiters)))

(defmethod initialize-instance :around ((h HL7Message) &key delimiters)
  (call-next-method h :delimiters delimiters))

(defun get-delimiters (s)
  (let ((ht (make-hash-table :test #'equalp)))
    (setf (gethash 'field ht) (elt s 3))
    (setf (gethash 'component ht) (elt s 4))
    (setf (gethash 'repeat ht) (elt s 5))
    (setf (gethash 'subcomponent ht) (elt s 6))
    (setf (gethash 'escape ht) (elt s 7))
    ht))

(defmethod initialize-instance :around ((h HL7Message) &key delimiters)
  (call-next-method h :delimiters delimiters))

(defclass HL7Segment ()
  ())

(defclass HL7Field ()
  ((delimiters :reader delimiters :initarg :delimiters)
  (value :reader value :initarg :value)
  (components :reader components)))

(defclass HL7Component ()
  ((delimiter :reader delimiter :initarg :delimiter)
  (value :reader value :initarg :value)
  (subcomponents :reader subcomponents)))

(defmethod initialize-instance :after ((c HL7Component) &key value delimiter)
  (let ((split (split-sequence:split-sequence delimiter value)))
    (with-slots (subcomponents) c
      (setf subcomponents (make-array (length split)
				      :element-type 'string
				      :initial-contents split)))))

(defmethod initialize-instance :after ((f HL7Field) &key value delimiters)
  (let ((split (split-sequence:split-sequence (gethash 'component delimiters) value))
	(subcomp (gethash 'subcomponent delimiters)))
    (with-slots (components) f
      (setf components (make-array (length split)
				   :element-type 'HL7Component
				   :initial-contents (mapcar (lambda (s) (make-instance 'HL7Component :value s :delimiter subcomp)) split))))))

(defgeneric insert-at (h index value))

(defmethod insert-at ((h HL7Component) index value)
  (with-slots (subcomponents) h
    (extend-and-insert subcomponents 'string "" index value)))

(defmethod insert-at ((h HL7Field) index (value HL7Component))
  (with-slots (components delimiters) h
    (extend-and-insert components 'HL7Component (make-instance 'HL7Component :value "" :delimiter (gethash 'subcomponent delimiters)) index value)))

(defun extend-and-insert (arr arr-type arr-init-element index value)
  (when (> index (length arr))
      (let ((new (make-array index :element-type arr-type :initial-element arr-init-element)))
	(replace new arr)
	(setf arr new)))
  (setf (aref arr (1- index)) value))
