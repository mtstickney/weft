;;;; package.lisp

(defpackage #:weft
  (:use #:cl)
  (:export #:task-manager
           #:manager-full-error
           #:add-task
           #:remove-task
           #:find-task
           #:stop-task
           #:all-tasks
           #:slots-available-p
           #:threaded-task-manager)
  (:export #:*shutdown*)
  (:export #:server
           #:server-task-manager
           #:server-socket
           #:server-address
           #:server-port
           #:server-connection-handler
           #:run
           #:stop
           #:stop-accepting)
  (:export #:thread-shutdown))
