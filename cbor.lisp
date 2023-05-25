(in-package es.packmat.bsky)

(defgeneric read-data-item (data &optional start)
  (:documentation "Read a single data item from DATA, starting at index START.
DATA can be a bit vector, a simple array (interpreted as an octet vector), or a string (interpreted as a hex string.)
Returns the deserialized data item and the next bit position."))

(defun %major-type->keyword (mt)
  (case mt
    (0 :unsigned)
    (1 :signed)
    (2 :byte-string)
    (3 :string)
    (4 :array)
    (5 :map)
    (6 :tag)
    (7 :simple-or-float)))

(-> %read-data-item (bit-vector integer &optional keyword) (values t number &optional))
(defun %read-data-item (bits start &optional whence)
  "Read a single data item from BITS; starting at index START, optionally using scope WHENCE (for error reporting).
Returns the deserialized data item and the next bit position."
  (when (< (- (length bits) start) 8)
    (error "malformed stream during ~A~%incomplete data item header!" whence))
  (let ((position start)
        (whence whence))
    (flet ((read-bits (count &key (lookahead nil))
             (if (< (- (length bits) position) count)
                 (error "malformed stream during ~A~%ran out of bits to read!" whence)
                 (let ((slice (sera:slice bits position
                                          (if lookahead
                                              (+ position count)
                                              (incf position count)))))
                   (sera:unbits slice :big-endian t)))))
      (let ((major-type (read-bits 3))
            (short-count (read-bits 5)))
        (flet ((handle-count ()
                 (case short-count
                   (24 (read-bits 8))
                   (25 (read-bits 16))
                   (26 (read-bits 32))
                   (27 (read-bits 64))
                   (t (if (and (> short-count 27)
                               (< short-count 31))
                          (v:error :cbor.read-data-item 'simple-error
                                   :format-control "28-30 must not be used")
                          short-count)))))
          (case major-type
            (0
             (setf whence :cbor.%read-data-item.unsigned)
             (values (handle-count) position))
            (1
             (setf whence :cbor.%read-data-item.signed)
             (values (- -1 (handle-count)) position))
            ((2 3)
             (if (= major-type 2)
                 (setf whence :cbor.%read-data-item.byte-string)
                 (setf whence :cbor.%read-data-item.string))
             (let ((len (handle-count)))
               (case short-count
                 (31 (let ((result (make-array 8 :initial-element 0
                                                 :fill-pointer 0
                                                 :adjustable t)))
                      (loop with cont = t
                            while cont
                            do (if (= 255 (read-bits 8 :lookahead T))
                                   (progn (incf position 8)
                                          (setf cont nil))
                                   (progn
                                     (let ((inner-mt (read-bits 3 :lookahead T)))
                                      (unless (= major-type inner-mt)
                                        (error "malformed chunk during ~A; expected ~A, got ~A"
                                               whence
                                               (%major-type->keyword major-type)
                                               (%major-type->keyword inner-mt))))
                                     (multiple-value-bind (data newpos)
                                         (%read-data-item bits position whence)
                                       (declare (ignorable data))
                                       (setf result (concatenate 'vector result data))
                                       (setf position newpos)))))
                       (values (if (= major-type 3)
                                   (concatenate 'string result)
                                   result)
                               position)))
                 (t (let ((data
                       (map 'vector
                            (lambda (i)
                              (declare (ignore i))
                              (read-bits 8))
                            (alex:iota len))))
                      (values (if (= major-type 3)
                                  (trivial-utf-8:utf-8-bytes-to-string data)
                                  data)
                              position))))))
            (4
             (setf whence :cbor.%read-data-item.array)
             (if (= short-count 31)
                 (let ((vector (make-array 8 :initial-element nil
                                             :fill-pointer 0
                                             :adjustable t)))
                   (loop with cont = t
                         while cont
                         do (if (= 255 (read-bits 8 :lookahead T))
                                (progn
                                  (incf position 8)
                                  (setf cont nil))
                                (multiple-value-bind (data newpos)
                                    (%read-data-item bits position whence)
                                  (setf position newpos)
                                  (vector-push-extend data vector))))
                   (values vector position))
                 
                 (let ((len (handle-count)))
                   (values (map 'vector
                                (lambda (i)
                                  (declare (ignore i))
                                  (multiple-value-bind (data newpos)
                                      (%read-data-item bits position whence)
                                    (setf position newpos)
                                    data))
                                (alex:iota len))
                           position))))
            (5
             (setf whence :cbor.%read-data-item.map)
             (if (= short-count 31)
                 (progn
                   (v:warn :cbor.%read-data-item "TODO: indefinite-length encoding"))
                 (let ((count (handle-count))
                       (table (make-hash-table :test 'equal)))
                   (mapcar (lambda (i)
                             (declare (ignore i))
                             (let (key value)
                               (multiple-value-bind (data newpos)
                                   (%read-data-item
                                    bits position :cbor.%read-data-item.map-key)
                                 (setf position newpos
                                       key data))
                               (multiple-value-bind (data newpos)
                                   (%read-data-item
                                    bits position :cbor.%read-data-item.map-value)
                                 (setf position newpos
                                       value data))
                               (sera:dict* table key value)))
                           (alex:iota count))
                   (values table position))))
            (t (values nil position))))))))

(defmethod read-data-item ((data bit-vector) &optional (start 0))
  (%read-data-item data start))
(defmethod read-data-item ((data string) &optional (start 0))
  (%read-data-item (bitsmash:hex->bits data) start))
(defmethod read-data-item ((data simple-array) &optional (start 0))
  (%read-data-item (bitsmash:octets->bits data) start)) 
