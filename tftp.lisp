(in-package #:org.shirakumo.tftp)

(defun port-in-use-p (port)
  NIL)

(defun random-port ()
  (+ 49152 (random (- 65535 49152))))

(defun find-port ()
  (loop for port = (random-port)
        do (unless (port-in-use-p port)
             (return port))))

(defun packet-string (packet start &key (end (length packet)))
  (let ((end (or (position 0 packet :start start :end end)
                 (error "Unterminated string in packet."))))
    (cond ((< start end)
           (let ((string (make-string (- end start) :element-type 'base-char)))
             (loop for i from start below end
                   for ascii = (aref packet i)
                   do (setf (char string (- i start)) (code-char ascii)))
             (values string (1+ end))))
          (T
           (values NIL (1+ end))))))

(defun (setf packet-string) (string packet start &key (end (length packet)))
  (when (< end (+ (length string) start 1))
    (cerror "Truncate the string" "The string~%  ~a~%is ~d characters long, which does not fit into the ~d remaining bytes in the packet."
            string (length string) (- end start)))
  (loop for i from start below end
        for char across string
        for code = (char-code char)
        do (setf (aref packet i) (restart-case (if (<= code 127) code (error "String part ~s is not ASCII." char))
                                   (continue ()
                                     :report "Replace the character with a question mark"
                                     (char-code #\?))
                                   (use-value (value)
                                     :report "Provide a replacement character"
                                     (char-code value)))))
  ;; Null terminate the string
  (let ((end (min (1- end) (+ start (length string)))))
    (setf (aref packet end) 0)
    (values string (1+ end))))

(defmacro define-packet-decoder (name offset &body parts)
  `(progn
     (defun ,name (packet)
       (ecase (nibbles:ub16ref/be packet ,offset)
         ,@parts))

     (defun (setf ,name) (value packet)
       (setf (nibbles:ub16ref/be packet ,offset)
             (ecase value
               ,@(loop for (code key) in parts
                       collect `((,key) ,code))))
       value)))

(define-packet-decoder packet-opcode 0
  (1 :rrq)
  (2 :wrq)
  (3 :data)
  (4 :ack)
  (5 :error)
  (6 :oack))

(define-packet-decoder packet-error-code 2
  (0 NIL)
  (1 :file-not-found)
  (2 :access-violation)
  (3 :disk-full)
  (4 :illegal-operation)
  (5 :unknown-transfer-id)
  (6 :file-exists)
  (7 :no-such-user))

(defun mode-code (mode)
  (cond ((string-equal mode "netascii") :ascii)
        ((string-equal mode "octet") :octet)
        ((string-equal mode "mail") :mail)
        (T (error "Unknown mode ~s" mode))))

(defun option-code (option)
  (cond ((string-equal option "timeout") :timeout)
        ((string-equal option "tsize") :transfer-size)
        ((string-equal option "blksize") :block-size)
        (T (warn "Unknown option ~s" option)
           NIL)))

(defun option-value (code value)
  (ecase code
    ((:timeout :transfer-size :block-size)
     (parse-integer value))))

(defun packet-error-message (packet)
  (packet-string packet 4))

(defun (setf packet-error-message) (value packet)
  (setf (packet-string packet 4) value))

(defun packet-blocknr (packet)
  (nibbles:ub16ref/be packet 2))

(defun (setf packet-blocknr) (blocknr packet)
  (setf (nibbles:ub16ref/be packet 2) blocknr))

(defun packet-strings (packet &key (start 2) (end (length packet)))
  (loop with i = start
        while (< i end)
        collect (multiple-value-bind (string end) (packet-string packet i :end end)
                  (setf i end)
                  string)))

(defun (setf packet-strings) (strings packet &key (start 2) (end (length packet)))
  (dolist (string strings (values strings start))
    (setf start (nth-value 1 (setf (packet-string packet start :end end) string)))))

(defun parse-options (options)
  (loop for cons on options by #'cddr
        for code = (option-code (first cons))
        for value = (when code (option-value code (second cons)))
        do (setf (first cons) code)
           (setf (second cons) value)))

(defun (setf packet-options) (options packet &key (start 2) (end (length packet)))
  (setf (packet-strings packet :start start :end end)
        (loop for (key val) on options by #'cddr
              when key collect (string key)
              when key collect (princ-to-string val))))

(define-condition tftp-error (error)
  ((code :initarg :code :initform NIL :reader code)
   (message :initarg :message :reader message))
  (:report (lambda (c s) (format s "~@[~a: ~]~a" (code c) (message c)))))

(defclass socket-object ()
  ((socket :initarg :socket :accessor socket)
   (verbose :initarg :verbose :initform NIL :accessor verbose)))

(defmethod host ((object socket-object))
  (usocket:host-to-hostname
   (usocket:get-local-address (socket object))))

(defmethod port ((object socket-object))
  (usocket:get-local-port (socket object)))

(defmethod log ((object socket-object) format &rest args)
  (when (verbose object)
    (if (slot-boundp object 'socket)
        (format *error-output* "~&TFTP ~a:~a> ~?~%" (host object) (port object) format args)
        (format *error-output* "~&TFTP CLOSED> ~?~%" format args))))

(defmethod close ((object socket-object) &key abort)
  (declare (ignore abort))
  (when (slot-boundp object 'socket)
    (log object "Closing")
    (usocket:socket-close (socket object))
    (slot-makunbound object 'socket))
  object)

(defclass server (socket-object)
  ((root :initarg :root :initform (make-pathname :directory '(:absolute)) :accessor root)
   (clients :initform () :accessor clients)))

(defmethod shared-initialize :after ((server server) slots &key (host "0.0.0.0") (port 69))
  (unless (slot-boundp server 'socket)
    (setf (socket server) (usocket:socket-connect NIL NIL :protocol :datagram :local-host host :local-port port))
    (log server "Listening for connections")))

(defmethod close :after ((server server) &key abort)
  (when abort
    (mapc #'close (clients server))
    (setf (clients server) ())))

(defmethod serve ((server server) &key (timeout 0))
  (let ((sockets ()))
    (labels ((build-socket-list ()
               (setf sockets (list* (socket server)
                                    (loop for client in (clients server)
                                          when (slot-boundp client 'socket)
                                          collect (socket client)))))
             (process (timeout)
               (dolist (socket (usocket:wait-for-input (build-socket-list) :timeout timeout))
                 (if (eql socket (socket server))
                     (let ((client (make-instance 'client :socket (socket server)
                                                          :root (root server)
                                                          :verbose (verbose server))))
                       (push client (clients server))
                       (push (socket client) sockets))
                     (let ((client (find socket (clients server) :key #'socket)))
                       (unwind-protect (serve client :timeout NIL)
                         (unless (slot-boundp client 'client)
                           (setf (clients server) (delete client (clients server)))
                           (build-socket-list))))))))
      (if (eql T timeout)
          (loop while (slot-boundp server 'socket)
                do (process 1.0))
          (process timeout))
      server)))

(defclass client (socket-object)
  ((root :initarg :root :initform (make-pathname :directory '(:absolute)) :accessor root)
   (blocknr :initform 0 :accessor blocknr)
   (block-size :initform 512 :accessor block-size)
   (transfer-size :initform NIL :accessor transfer-size)
   (timeout :initform 60 :accessor timeout)
   (src-port :initarg :src-port :initform (find-port) :accessor src-port)
   (dst-port :initarg :dst-port :initform 69 :accessor dst-port)
   (packet :initform (make-array (+ 512 4) :element-type '(unsigned-byte 8)) :accessor packet)
   (output :accessor output)))

(defmethod initialize-instance :after ((client client) &key host (port (dst-port client)))
  (cond ((slot-boundp client 'socket)
         (log client "Accepted remote connection")
         ;; We are in receiving client mode. Handle the connection now.
         (serve client))
        (T
         ;; We are in initiating client mode. Create a socket and wait for the user to initiate a request.
         (setf (socket client) (usocket:socket-connect host port :protocol :datagram :local-port (src-port client)))
         (log client "Connected to server"))))

(defmethod close :before ((client client) &key abort)
  (when (slot-boundp client 'output)
    (close (output client) :abort abort)
    (slot-makunbound client 'output)))

(defmethod host ((object client))
  (usocket:host-to-hostname
   (usocket:get-peer-address (socket object))))

(defmethod port ((object client))
  (usocket:get-peer-port (socket object)))

(defmethod send ((client client) size)
  (usocket:socket-send (socket client) (packet client) size))

(defmethod fail ((client client) code message &rest args)
  (let ((packet (packet client))
        (message (format NIL "~?" message args)))
    (setf (packet-opcode packet) :error)
    (setf (packet-error-code packet) code)
    (send client (nth-value 1 (setf (packet-error-message packet) message)))
    (log client "Failed: ~a" message)
    (close client)
    (error 'tftp-error :code code :message message)))

(defmethod (setf block-size) :after (size (client client))
  ;; Adjust packet size to at least hold one full block.
  (let ((packet-size (+ 4 (block-size client))))
    (when (< (length (packet client)) packet-size)
      (setf (packet client) (replace (make-array packet-size :element-type '(unsigned-byte 8))
                                     (packet client))))))

(defmethod serve ((client client) &key (timeout (timeout client)))
  (restart-case
      (labels ((process (timeout)
                 (if (usocket:wait-for-input (socket client) :timeout timeout)
                     (let* ((packet (packet client))
                            (read (usocket:socket-receive (socket client) packet (length packet))))
                       (when (< 0 read)
                         (handle-packet client (packet-opcode packet) packet read)))
                     (error "RETRANSMIT"))))
        (if (eql T timeout)
            (loop while (slot-boundp client 'socket)
                  do (process (timeout client)))
            (process timeout))
        client)
    (abort ()
      :report "Close the client."
      (close client))))

(defmethod handle-options ((client client) options &key ack)
  (setf options (parse-options options))
  (loop for (option value) on options
        do (ecase option
             (:timeout (setf (timeout client) value))
             (:transfer-size (setf (transfer-size client) value))
             (:block-size (setf (block-size client) value))
             ((NIL))))
  (log client "Negotiated for~{ ~a: ~a~^,~}" options)
  (when ack
    (let ((packet (packet client)))
      (setf (packet-opcode packet) :oack)
      (send client (nth-value 1 (setf (packet-options packet) options))))))

(defmethod handle-packet ((client client) (op (eql :rrq)) packet end)
  (destructuring-bind (filename mode . options) (packet-strings packet :end end)
    (case (mode-code mode)
      (:mail (fail client :illegal-operation "Mail transfer mode is unsupported.")))
    (log client "Read request for ~a" filename)
    (handle-options client options :ack T)
    (handle-packet client op filename mode)))

(defmethod handle-packet ((client client) (op (eql :wrq)) packet end)
  (destructuring-bind (filename mode . options) (packet-strings packet :end end)
    (case (mode-code mode)
      (:mail (fail client :illegal-operation "Mail transfer mode is unsupported.")))
    (log client "Write request for ~a" filename)
    (handle-options client options :ack T)
    (handle-packet client op filename mode)))

(defmethod handle-packet ((client client) (op (eql :oack)) packet end)
  (handle-options client (packet-strings packet))
  (setf (packet-opcode packet) :ack)
  (setf (packet-blocknr packet) 0)
  (send client 4))

(defmethod handle-packet ((client client) (op (eql :error)) packet end)
  (close client)
  (error 'tftp-error :code (packet-error-code packet)
                     :message (packet-error-message packet)))

(defmethod handle-packet ((client client) (op (eql :ack)) packet end)
  (let ((blocknr (packet-blocknr packet)))
    (when (/= blocknr (blocknr client))
      (fail client :illegal-operation "Bad block number in ACK, expected ~d but got ~d" (blocknr client) blocknr))
    (setf (packet-opcode packet) :data)
    (setf (packet-blocknr packet) (incf (blocknr client)))
    (let ((end (read-sequence packet (output client) :start 4 :end (+ 4 (block-size packet)))))
      (send client end)
      (log client "Sent block of ~d bytes" (- end 4)))))

(defmethod handle-packet ((client client) (op (eql :data)) packet end)
  (let ((blocknr (packet-blocknr packet)))
    (when (/= blocknr (1+ (blocknr client)))
      (fail client :illegal-operation "Bad block number in DATA, expected ~d but got ~d" (1+ (blocknr client)) blocknr))
    (write-sequence packet (output client) :start 4 :end end)
    (setf (packet-opcode packet) :ack)
    (setf (packet-blocknr packet) (incf (blocknr client)))
    (send client 4)
    (log client "Received block of ~d bytes" (- end 4))
    (when (< (- end 4) (block-size client))
      (close client))))

(defmethod handle-packet ((client client) (op (eql :rrq)) (filename string) mode)
  (handler-case
      (let* ((path (merge-pathnames filename (root client)))
             (stream (open path :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist NIL)))
        (unless stream
          (fail client :file-not-found "No such file ~a" filename))
        (setf (output client) stream)
        ;; The fake ACK will cause the transfer to start.
        (handle-packet client :ack #(0 0 0 0) 4))
    (file-error ()
      (fail client :access-violation "Failed to access ~a" filename))))

(defmethod handle-packet ((client client) (op (eql :wrq)) (filename string) mode)
  (handler-case
      (let* ((path (merge-pathnames filename (root client)))
             (stream (open path :direction :output :element-type '(unsigned-byte 8) :if-exists NIL))
             (packet (packet client)))
        (unless stream
          (fail client :file-exists "The file already exists at ~a" filename))
        (setf (output client) stream)
        (setf (packet-opcode packet) :ack)
        (setf (packet-blocknr packet) 0)
        (send client 4))
    (file-error ()
      (fail client :access-violation "Failed to access ~a" filename))))

(defmethod put ((client client) (src stream) &key filename block-size (timeout (timeout client)) (synchronous T))
  (let ((packet (packet client)))
    (setf (timeout client) timeout)
    (setf (packet-opcode packet) :wrq)
    (let* ((start (nth-value 1 (setf (packet-strings packet) (list filename "octet"))))
           (options (list :transfer-size (when (typep src 'file-stream) (file-length src))
                          :block-size (or block-size (- 4 (length packet)))
                          :timeout (timeout client))))
      (setf (output client) src)
      (send client (nth-value 1 (setf (packet-options packet :start start) options)))))
  (when synchronous
    (serve client :timeout T))
  client)

(defmethod get ((client client) (dst stream) &key filename block-size (timeout (timeout client)) (synchronous T))
  (let ((packet (packet client)))
    (setf (timeout client) timeout)
    (setf (packet-opcode packet) :rrq)
    (let ((start (nth-value 1 (setf (packet-strings packet) (list filename "octet"))))
          (options (list :transfer-size 0
                         :block-size (or block-size (- 4 (length packet)))
                         :timeout (timeout client))))
      (setf (output client) dst)
      (send client (nth-value 1 (setf (packet-options packet :start start) options)))))
  (when synchronous
    (serve client :timeout T))
  client)

(defmethod put ((client client) (file pathname) &rest args &key (if-does-not-exist :error) (filename (file-namestring file)) &allow-other-keys)
  (let ((stream (open file :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist if-does-not-exist)))
    (when stream
      (apply #'put client stream :filename filename args))))

(defmethod get ((client client) (file pathname) &rest args &key (if-exists :error) (filename (file-namestring file)) &allow-other-keys)
  (let ((stream (open file :direction :output :element-type '(unsigned-byte 8) :if-exists if-exists)))
    (when stream
      (apply #'get client stream :filename filename args))))

(defmethod put ((hostname string) file &rest args &key (port 69) &allow-other-keys)
  (remf args :port)
  (apply #'put (make-instance 'client :host hostname :port port) file args))

(defmethod get ((hostname string) file &rest args &key (port 69) &allow-other-keys)
  (remf args :port)
  (apply #'get (make-instance 'client :host hostname :port port) file args))

(defmethod serve ((root pathname) &rest args &key (timeout T) &allow-other-keys)
  (remf args :timeout)
  (serve (apply #'make-instance 'server :root root args) :timeout timeout))
