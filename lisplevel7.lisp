(in-package #:lisplevel7)

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Backspace #\Linefeed #\Page #\Return #\Rubout))

(defclass HL7Delimiters ()
  ((field :accessor field :initarg :field :initform #\|)
   (component :accessor component :initarg :component :initform #\^)
   (repeat :accessor repeat :initarg :repeat :initform #\~)
   (subcomponent :accessor subcomponent :initarg :subcomponent :initform #\&)
   (escape :accessor escape :initarg :escape :initform #\\)))

(defclass HL7Root ()
  ((value :reader value :initarg :value)
   (delimiters :reader delimiters)))

(defmethod print-object ((obj HL7Root) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (value obj))))

(defgeneric insert-at (h index value))

(defun extend-and-insert (arr arr-type arr-init-element index value)
  (when (> index (length arr))
      (let ((new (make-array index :element-type arr-type :initial-element arr-init-element)))
	(replace new arr)
	(setf arr new)))
  (setf (aref arr (1- index)) value))

;------------HL7Message------------

(defclass HL7Message (HL7Root)
  ((segments :reader segments)))

(defmethod initialize-instance :after ((h HL7Message) &key value)
  (let* ((stripped (remove-if-not #'standard-char-p value))
	 (split (split-sequence:split-sequence #\Newline stripped)))
    (with-slots (delimiters segments) h
      (setf delimiters (read-delimiters stripped))
      (setf segments (make-array (length split)
				 :element-type 'HL7Segment
				 :initial-contents (mapcar (lambda (s) (make-instance 'HL7Segment :value s :delimiters delimiters)) split))))))

(defun read-delimiters (s)
  (make-instance 'HL7Delimiters
      :field (elt s 3)
      :component (elt s 4)
      :repeat (elt s 5)
      :subcomponent (elt s 6)
      :escape (elt s 7)))

;------------HL7Segment------------

(defclass HL7Segment (HL7Root)
  ((fields :reader fields)))

(defmethod initialize-instance :after ((s HL7Segment) &key value delimiters)
  (with-slots (field repeat) delimiters
    (let ((split (split-sequence:split-sequence field value)))
      (with-slots (fields) s
	(setf fields (make-array (length split)))
	(loop for f in split for i upto (length split) do
	     (let ((repeats (split-sequence:split-sequence repeat f)))
	       (setf (elt fields i) (make-array (length repeats)
						:element-type 'HL7Field
						:initial-contents (mapcar (lambda (s) (make-instance 'HL7Field :value s :delimiters delimiters)) repeats)))))))))

;------------HL7Field------------

(defclass HL7Field (HL7Root)
  ((components :reader components)))

(defmethod initialize-instance :after ((f HL7Field) &key value delimiters)
  (with-slots (component) delimiters
  (let ((split (split-sequence:split-sequence component value)))
    (with-slots (components) f
      (setf components (make-array (length split)
				   :element-type 'HL7Component
				   :initial-contents (mapcar (lambda (s) (make-instance 'HL7Component :value s :delimiters delimiters)) split)))))))

(defmethod insert-at ((h HL7Field) index (value HL7Component))
  (with-slots (components delimiters) h
    (extend-and-insert components 'HL7Component (make-instance 'HL7Component :value "" :delimiters delimiters) index value)))

;------------HL7Component------------

(defclass HL7Component (HL7Root)
  ((subcomponents :reader subcomponents)))

(defmethod initialize-instance :after ((c HL7Component) &key value delimiters)
  (with-slots (subcomponent) delimiters
    (let ((split (split-sequence:split-sequence subcomponent value)))
      (with-slots (subcomponents) c
	(setf subcomponents (make-array (length split)
					:element-type 'string
					:initial-contents split))))))

(defmethod insert-at ((h HL7Component) index value)
  (with-slots (subcomponents) h
    (extend-and-insert subcomponents 'string "" index value)))
