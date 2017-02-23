;;;; weft.lisp

(in-package #:weft)

(defvar *id-counter* 0)
(let ((id-lock (bordeaux-threads:make-lock)))
  (defun gen-id ()
    (bordeaux-threads:with-lock-held (id-lock)
      (prog1
          (make-symbol
           (concatenate 'string "TASK"
                        (write-to-string *id-counter*
                                         :base 10
                                         :radix nil)))
        (incf *id-counter*)))))

(defclass task-manager ()
  ())

(defgeneric add-task (manager thunk)
  (:documentation "Add a task to execute THUNK to MANAGER. Returns a unique ID for the task."))

(defgeneric remove-task (manager task)
  (:documentation "Remove TASK from the task-manager MANAGER."))

(defgeneric find-task (manager task-id)
  (:documentation "Return the entry for TASK-ID from MANAGER."))

(defgeneric stop-task (manager task)
  (:documentation "Perform shutdown of the TASK in MANAGER."))

(defgeneric all-tasks (manager)
  (:documentation "Return a list of the ids of all tasks in MANAGER."))

(defclass threaded-task-manager (task-manager)
  ((tasks :initform '() :accessor tasks)
   (task-lock :initform (bordeaux-threads:make-recursive-lock) :reader task-lock)
   (shutdown-vars :initform (make-hash-table :test #'equal) :accessor shutdown-vars))
  (:documentation "Class implementing a thread-per-connection task manager."))

(define-condition task-exists-error ()
  ((id :initarg :id :accessor task-id))
  (:documentation "Error thrown when a task with a particular ID already exists."))

(defmethod find-task ((manager threaded-task-manager) (task symbol))
  ;; string= is used to compare uninterned symbols by name (symbols
  ;; are coerced to strings). Why aren't we just using strings, then?
  (assoc task (tasks manager) :test #'string=))

(defun prompt-for (type format-string &rest format-args)
  "Prompt for a value of type TYPE, using FORMAT-STRING and FORMAT-ARGS to produce the prompt. Signals an error if the value read is not of type TYPE."
  (apply #'format *query-io* format-string format-args)
  (let ((val (read *query-io*)))
    (if (not (typep val type))
        (error 'type-error :expected-type type :datum val)
        val)))

;; TODO: Move this to utilities
#+sbcl
(defun size-of (obj)
  (flet ((round-to-dualword (size)
           (logand (the sb-vm:word (+ size sb-vm:lowtag-mask))
                   (lognot sb-vm:lowtag-mask))))
    (etypecase obj
      ((or structure-object standard-object)
       (round-to-dualword (* (+ (sb-kernel:%instance-length obj) 1)
                             sb-vm:n-word-bytes)))
      (cons
       (* 2 sb-vm:n-word-bytes)))))

(defstruct ref
  (val))

(define-symbol-macro *shutdown* (ref-val *shutdown-slot*))

(defgeneric task-shutdown-p (manager task-id)
  (:documentation "Return a boolean indicating whether the task identified by TASK-ID in MANAGER should shut down.")
  (:method ((manager threaded-task-manager) (task-id symbol))
    (ref-val (gethash (symbol-name task-id) (shutdown-vars manager)))))

(defgeneric (setf task-shutdown-p) (new-val manager task-id)
  (:documentation "Set the shutdown variable for the task identified by TASK-ID in MANAGER to NEW-VAL.")
  (:method (new-val (manager threaded-task-manager) (task-id symbol))
    (check-type new-val boolean)
    (setf (ref-val (gethash (symbol-name task-id) (shutdown-vars manager)))
          new-val)))

(defmethod add-task ((manager threaded-task-manager) thunk)
  (let* ((id (gen-id))
         (stdout *standard-output*))
    ;; Create a shutdown slot for this thread
    (setf (gethash (symbol-name id) (shutdown-vars manager))
          (make-ref :val nil))
    (log:debug "Shutdown var is" (gethash (symbol-name id) (shutdown-vars manager)))
    (bordeaux-threads:with-recursive-lock-held ((task-lock manager))
      (setf (tasks manager)
            (acons id (bordeaux-threads:make-thread
                       (lambda ()
                         (let* ((*task-id* id)
                                (*shutdown-slot* (gethash (symbol-name *task-id*)
                                                          (shutdown-vars manager)))
                                (*standard-output* stdout))
                           (declare (special *task-id* *shutdown-slot*))
                           (unwind-protect
                                (handler-case (funcall thunk)
                                  (thread-shutdown (c)
                                    (declare (ignore c))))
                             (log:debug "Thread in cleanup")
                             (remove-task manager *task-id*)))))
                   (tasks manager))))
    id))

(defmethod stop-task ((manager threaded-task-manager) (task symbol))
  (let ((entry (find-task manager task)))
    (if (or (null entry)
            (null (cdr entry))
            (not (bordeaux-threads:thread-alive-p (cdr entry))))
        nil
        (progn
          (setf (task-shutdown-p manager task) t)))))

(defmethod remove-task ((manager threaded-task-manager) (task symbol))
  (let ((entry (find-task manager task)))
    (if entry
        (progn
          (bordeaux-threads:with-recursive-lock-held ((task-lock manager))
            (setf (tasks manager) (remove entry (tasks manager)))
            ;; Remove the shutdown slot for this thread
            (remhash (symbol-name task) (shutdown-vars manager)))
          t)
        nil)))

(defmethod all-tasks ((manager threaded-task-manager))
  (mapcar #'car (tasks manager)))

(defun connection-handler-func (server sock handler)
  "Return a wrapper func for HANDLER that will ensure SOCK is closed at exit."
  (check-type server server)
  (check-type sock usocket:stream-usocket)
  (check-type handler (or function symbol))
  (lambda ()
    (unwind-protect
         (apply handler (cons sock (server-handler-args server)))
      (usocket:socket-close sock))))

;; TODO: use task-shutdown-p in more places
;; TODO: fix object indirect reference problem with *SHUTDOWN-SLOT*
(defun acceptor-func (server)
  (check-type server server)
  (lambda ()
    (declare (special *shutdown-slot*))
    (log:debug "Acceptor starting." *shutdown*)
    (unwind-protect
         (loop while (not *shutdown*)
            with socket = (server-socket server)
            do (progn
                 (log:debug "Doing accept loop")
                 (log:debug *shutdown*)
                 (log:debug *shutdown-slot*)
                 (usocket:wait-for-input socket :timeout 2)
                 (when (eq (usocket::state socket) :read)
                   (let ((sock (usocket:socket-accept
                                (server-socket server))))
                     (add-task (server-task-manager server)
                               (connection-handler-func
                                server
                                sock
                                (server-connection-handler server)))))))
      (log:debug "In acceptor cleanup")
      (usocket:socket-close (server-socket server))
      (setf (server-socket server) nil)
      (values))))

(defclass server ()
  ((task-manager :initarg :manager :accessor server-task-manager)
   (socket :initform nil :accessor server-socket)
   (address :initarg :address :accessor server-address)
   (port :initarg :port :accessor server-port)
   (acceptor-task :initform nil :accessor server-acceptor-task)
   (connection-handler :initarg :handler
                       :accessor server-connection-handler)
   (handler-args :initarg :args :accessor server-handler-args))
  (:default-initargs
   :manager (make-instance 'threaded-task-manager)
    :args '()))

(define-condition thread-shutdown () ())

;; TODO: stop-accepting should probably be a :SOFT keyarg for STOP
(defgeneric run (server &key backlog element-type))
(defgeneric stop (server))
(defgeneric stop-accepting (server))

(defmethod run ((server server) &key (backlog 5) (element-type 'character))
  (when (server-socket server)
    (usocket:socket-close (server-socket server)))
  (log:info "Starting server" server)
  (setf (server-socket server)
        (prog1 (usocket:socket-listen (server-address server)
                                      (server-port server)
                                      :backlog backlog
                                      :element-type element-type)
          (log:debug "Server socket created"))
        (server-acceptor-task server)
        (add-task (server-task-manager server)
                  (acceptor-func server)))
  (values))

(defmethod stop ((server server))
  (log:info "Stopping server" server)
  (let ((tasks (all-tasks (server-task-manager server))))
    (mapc #'(lambda (task)
              (stop-task (server-task-manager server)
                         task))
          tasks)
    (values)))

(defmethod stop-accepting ((server server))
  (log:info "Refusing new connections to server" server)
  (let ((task (server-acceptor-task server)))
    (when task
      (stop-task (server-task-manager server)
                 task))))

;;; "weft" goes here. Hacks and glory await!
