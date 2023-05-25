(in-package es.packmat.bsky)

(defun read-count (bits short-count start)
  "Reads an extended count, if applicable, based on the value of SHORT-COUNT.
Returns either a parsed extended count field or the original short count, and the next bit position."
  (let ((position start))
    (values (case short-count
              (24 (sera:unbits (sera:slice bits position (incf position 8))
                               :big-endian t))
              (25 (sera:unbits (sera:slice bits position (incf position 16))
                               :big-endian t))
              (26 (sera:unbits (sera:slice bits position (incf position 32))
                               :big-endian t))
              (27 (sera:unbits (sera:slice bits position (incf position 64))
                               :big-endian t))
              (t (if (and (> short-count 27)
                          (< short-count 31))
                     (v:error :cbor.read-data-item 'simple-error
                              :format-control "28-30 must not be used")
                     short-count)))
            position)))

(defgeneric read-data-item (data &optional start)
  (:documentation "Read a single data item from DATA, starting at index START.
DATA can be a bit vector, a simple array (interpreted as an octet vector), or a string (interpreted as a hex string.)
Returns the deserialized data item and the next bit position."))

(-> %read-data-item (bit-vector integer) (values t number &optional))
(defun %read-data-item (bits start)
  "Read a single data item from BITS; starting at index START.
Returns the deserialized data item and the next bit position."
  (when (< (- (length bits) start) 8)
    (error "Malformed stream; incomplete data item header!"))
  (let* ((position start)
         (major-type  (sera:unbits (sera:slice bits position (incf position 3))
                                   :big-endian t))
         (short-count (sera:unbits (sera:slice bits position (incf position 5))
                                   :big-endian t)))
    (case major-type
      (0 (read-count bits short-count position))
      (1 (multiple-value-bind (signum pos)
             (read-count bits short-count position)
           (values (- -1 signum) pos)))
      ((2 3) (let (len)
               (multiple-value-bind (ext-count newpos)
                   (read-count bits short-count position)
                 (setf position newpos
                       len ext-count))
               (let ((data
                       (map 'vector
                            (lambda (i)
                              (declare (ignore i))
                              (sera:unbits (sera:slice bits position (incf position 8))
                                           :big-endian t))
                            (alex:iota len))))
                 (values (if (= major-type 3)
                             (trivial-utf-8:utf-8-bytes-to-string data)
                             data)
                         position))))
      (4 (if (= short-count 31)
             (progn
               (v:warn :cbor.%read-data-item "TODO: indefinite-length encoding"))
             (let (len)
               (multiple-value-bind (ext-count newpos)
                   (read-count bits short-count position)
                 (setf position newpos
                       len ext-count))
               (values (map 'vector
                            (lambda (i)
                              (declare (ignore i))
                              (multiple-value-bind (data newpos)
                                  (read-data-item bits position)
                                (setf position newpos)
                                data))
                            (alex:iota len))
                       position))))
      (t (values nil position)))))

(defmethod read-data-item ((data bit-vector) &optional (start 0))
  (%read-data-item data start))
(defmethod read-data-item ((data string) &optional (start 0))
  (%read-data-item (bitsmash:hex->bits data) start))
(defmethod read-data-item ((data simple-array) &optional (start 0))
  (%read-data-item (bitsmash:octets->bits data) start)) 
