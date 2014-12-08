(in-package #:lisplevel7)

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Backspace #\Linefeed #\Page #\Return #\Rubout))

;------------Class defs------------

(defclass HL7Root ()
  ((value :reader value :initarg :value)
   (delimiters :reader delimiters :initarg :delimiters)))

(defclass HL7Message (HL7Root)
  ((segments :reader segments)))

(defclass HL7Segment (HL7Root)
  ((fields :reader fields)))

(defclass HL7Field (HL7Root)
  ((components :reader components)))

(defclass HL7Component (HL7Root)
  ((subcomponents :reader subcomponents)))

(defclass HL7Delimiters ()
  ((field :accessor field :initarg :field :initform #\|)
   (component :accessor component :initarg :component :initform #\^)
   (repeat :accessor repeat :initarg :repeat :initform #\~)
   (subcomponent :accessor subcomponent :initarg :subcomponent :initform #\&)
   (escape :accessor escape :initarg :escape :initform #\\)))

(defmethod print-object ((obj HL7Delimiters) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (field component repeat subcomponent escape) obj
      (format out "field:~a component:~a repeat:~a subcomponent:~a escape:~a" field component repeat subcomponent escape))))

(defmethod print-object ((obj HL7Root) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (value obj))))

(defgeneric insert-at (h index value))
(defgeneric value (h))
(defgeneric query (h position))

(defun extend-and-insert (arr arr-type arr-init-element index value)
  (when (> index (length arr))
      (let ((new (make-array index :element-type arr-type :initial-element arr-init-element)))
	(replace new arr)
	(setf arr new)))
  (setf (aref arr (1- index)) value))

(defun split (s delimiter)
  (split-sequence:split-sequence delimiter s))

(defun delimit (sequence delimiter)
  (with-output-to-string (s)
    (iterate:iterate (iterate:for entry in-sequence sequence) (iterate:for i from 0 to (length sequence))
		     (format s "~a" entry)
		     (when (< i (1- (length sequence)))
		       (format s "~a" delimiter)))))

;------------HL7Message------------

(defmethod initialize-instance :after ((h HL7Message) &key value)
  (let* ((stripped (remove-if-not #'standard-char-p value))
	 (split-string (split #\Newline stripped)))
    (with-slots (delimiters segments) h
      (setf delimiters (read-delimiters stripped))
      (setf segments (make-array (length split-string)
				 :element-type 'HL7Segment
				 :initial-contents (mapcar (lambda (s) (make-instance 'HL7Segment :value s :delimiters delimiters)) split-string))))))

(defun read-delimiters (s)
  (make-instance 'HL7Delimiters
      :field (elt s 3)
      :component (elt s 4)
      :repeat (elt s 5)
      :subcomponent (elt s 6)
      :escape (elt s 7)))

(defmethod value ((h HL7Message))
  (with-slots (segments) h
    (delimit segments #\Newline)))

;------------HL7Segment------------

(defmethod initialize-instance :after ((s HL7Segment) &key value delimiters)
  (with-slots (field repeat) delimiters
    (let ((split-string (split field value)))
      (with-slots (fields) s
	(setf fields (make-array (length split-string)))
	(loop for f in split-string for i upto (length split-string) do
	     (let ((repeats (split repeat f)))
	       (setf (elt fields i) (make-array (length repeats)
						:element-type 'HL7Field
						:initial-contents (mapcar (lambda (s) (make-instance 'HL7Field :value s :delimiters delimiters)) repeats)))))))))

(defmethod insert-at ((h HL7Segment) index (value string))
  (with-slots (delimiters) h
    (insert-at h index (make-instance 'HL7Field :value value :delimiters delimiters))))

(defmethod insert-at ((h HL7Segment) index (value HL7Field))
  (with-slots (fields delimiters) h
    (when (> index (length fields))
      (let ((new (make-array index)))
	(replace new fields)
	(setf fields new)))
    (let* ((field (aref fields index))
	   (new (make-array (1+ (length field)) :element-type 'HL7Field :initial-element (make-instance 'HL7Field :value "" :delimiters delimiters))))
      (replace new field)
      (setf field new)
      (setf (aref field (1- (length field))) value)
      (setf (aref fields index) field))))

(defmethod value ((h HL7Segment))
  (with-slots (fields delimiters) h
    (let ((field-delimiter (field delimiters)))
      (delimit fields field-delimiter))))

;------------HL7Field------------

(defmethod initialize-instance :after ((f HL7Field) &key value delimiters)
  (with-slots (component) delimiters
  (let ((split-string (split component value)))
    (with-slots (components) f
      (setf components (make-array (length split-string)
				   :element-type 'HL7Component
				   :initial-contents (mapcar (lambda (s) (make-instance 'HL7Component :value s :delimiters delimiters)) split-string)))))))

(defmethod insert-at ((h HL7Field) index (value HL7Component))
  (with-slots (components delimiters) h
    (extend-and-insert components 'HL7Component (make-instance 'HL7Component :value "" :delimiters delimiters) index value)))

(defmethod value ((h HL7Field))
  (with-slots (components delimiters) h
    (let ((compdelimiter (component delimiters)))
      (delimit components compdelimiter))))

;------------HL7Component------------

(defmethod initialize-instance :after ((c HL7Component) &key value delimiters)
  (with-slots (subcomponent) delimiters
    (let ((split-string (split subcomponent value)))
      (with-slots (subcomponents) c
	(setf subcomponents (make-array (length split-string)
					:element-type 'string
					:initial-contents split-string))))))

(defmethod insert-at ((h HL7Component) index value)
  (with-slots (subcomponents) h
    (extend-and-insert subcomponents 'string "" index value)))

(defmethod value ((h HL7Component))
  (with-slots (subcomponents delimiters) h
    (let ((subcompdelimiter (subcomponent delimiters)))
      (delimit subcomponents subcompdelimiter))))
