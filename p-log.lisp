(in-package :p-log)

(defparameter *log-severity-map* (list :error 3 :warn 2 :info 1 :debug 0))
(defparameter *reserved-log-keys* '(:message :timestamp :severity))
(defparameter *log-mutex* (make-mutex :name "p-log"))
;; A list of log sinks that look like this:
;;
;;    '(:name name
;;      :filename filename
;;      :stream stream
;;      :is-file is-file
;;      :format format
;;      :severity-threshold severity)
;;
;; :name                The name of the log sink, so that it can be closed
;;                      later.
;; :filename            A filename. Can be NIL.
;; :stream              A stream. Never NIL.
;; :is-file             A boolean value that indicates if the stream is
;;                      associated with a file.
;; :format              Either :jsonl or :plain.
;; :severity-threshold  A value (not keyword) from *LOG-SEVERITY-MAP*.
;;
;; The log functions output to every open log sink in this list.
;;
;; To add a sink to the list, use the OPEN-LOG function.
;;
;; To remove a sink from the list, use the CLOSE-LOG function.
(defparameter *logs* nil)

(defun log-sink-unique-p (name file-or-stream)
  "Helper function to determine if NAME or FILE-STREAM are already taken."
  (cond
    ((member
       name
       (mapcar (lambda (s) (getf s :name)) *logs*)
       :test 'equal)
      (perror :in "log-sink-unique-p"
        :status "log sink name already exists"
        :name name)
      nil)
    ((and
       (streamp file-or-stream)
       (member file-or-stream (mapcar (lambda (s) (getf s :stream)) *logs*)
         :test 'equal))
      (perror :in "log-sink-unique-p"
        :status "log sink stream already exists"
        :name name)
      nil)
    ((and
       (null file-or-stream)
       (member *standard-output* (mapcar (lambda (s) (getf s :stream)) *logs*)))
      (perror :in "log-sink-unique-p"
        :status "log sink stream already exists"
        :name name)
      nil)
    (t t)))

(defun create-file-stream-sink (name file log-format append severity-threshold)
  "Internal helper function for MAKE-LOG-STREAM."
  (cond
    ;; Check log-format
    ((not (member log-format '(:jsonl :plain)))
      (perror :in "create-file-stream-sink" :status "invalid log-format value"
        :name name :log-format log-format)
      nil)
    ;; Check append parameter
    ((not (member append '(nil t)))
      (perror :in "create-file-stream-sink" :status "append value not boolean"
        :name name :append append)
      nil)
    ;; Check severity threshold
    ((not (getf *log-severity-map* severity-threshold))
      (perror :in "create-file-stream-sink"
        :status "invalid severity-threshold value"
        :name name :severity-threshold severity-threshold)
      nil)
    (t (let* ((stream (handler-case
                        (open (parse-namestring file)
                          :direction :output
                          :if-exists (if append :append :supersede)
                          :if-does-not-exist :create)
                        (error (e)
                          (plog :error
                            (list :in "create-file-stream-sink"
                              :status "failed to open file"
                              :name name :filename file
                              :error (format nil "~a" e)))
                          nil)))
               (sink (when stream
                       (list
                         :name name
                         :filename file
                         :stream stream
                         :is-file t
                         :format log-format
                         :severity-threshold
                         (getf *log-severity-map* severity-threshold)))))
         (when sink
           (push sink *logs*)
           sink)))))

(defun create-stream-sink (name stream log-format severity-threshold)
  "Internal helper function for MAKE-LOG-STREAM."
  (cond
    ;; Check log-format
    ((not (member log-format '(:jsonl :plain)))
      (perror :in "create-stream-sink" :status "invalid log-format value"
        :name name :log-format log-format)
      nil)
    ;; Check severity threshold
    ((not (getf *log-severity-map* severity-threshold))
      (perror :in "create-stream-sink"
        :status "invalid severity-threshold value"
        :name name :severity-threshold severity-threshold)
      nil)
    (t (let ((sink (list
                     :name name
                     :filename nil
                     :stream stream
                     :is-file nil
                     :format log-format
                     :severity-threshold
                     (getf *log-severity-map* severity-threshold))))
         (push sink *logs*)
         sink))))

(defun make-log-stream (name file-or-stream &key
                         (log-format :jsonl)
                         (append t)
                         (severity-threshold :debug))
  "Opens a log file or sets up logging to a specified stream, adding a log
sink into the *LOGS* list. The P-LOG function and other logging function write
logs to every sink in the list. If *LOGS* is empty, then the logging functions
are similar to no-ops.

NAME must be a string that uniquely identifies the log sink.

FILE-OR-STREAM must be a string, a pathname, a stream, or NIL. If FILE-OR-STREAM
is a string or a pathname, it is treated as a file path, and this function opens
the file and uses that as the stream. If FILE-OR-STREAM is a stream, such as
*STANDARD-OUTPUT*, then this function uses it directly. Finally, if
FILE-OR-STREAM is NIL, then it defaults to *STANDARD-OUTPUT*.

LOG-FORMAT can be :jsonl (where each line is a JSON object) or
:plain (traditional, plain text log line). LOG-FORMAT defaults to :jsonl.

APPEND indicates that if a file exists at FILE-OR-STREAM, calls to P-LOG should
append log entries to the end of the existing file. If APPEND is NIL, the file
at FILE-OR-STREAM is cleared. Regardless of the value of APPEND, if the file at
FILE-OR-STREAM doesn't exist, this function creates it. If FILE-OR-STREAM is
a stream, APPEND has not effect.

SEVERITY-THRESHOLD is :debug, :info, :warn, or :error. A log entry is written
to a log sink only when the log entry's severity is greater than or equal to
the log sink's severity threshold.

Use the CLOSE-LOG-STREAM function to remove a log sink from the log sink
list (*LOGS*) and close the associated log stream. Alternatively, you can close
all the log sinks with CLOSE-LOG-SINKS

After adding the newly created log sink record to *LOGS*, this function returns
the new record. If the function fails to create the new record, it returns NIL.
"
  (log-sink-unique-p name file-or-stream)
  (cond
    ((stringp file-or-stream)
      (create-file-stream-sink name file-or-stream log-format append
        severity-threshold))
    ((streamp file-or-stream)
      (create-stream-sink name file-or-stream log-format
        severity-threshold))
    ((null file-or-stream)
      (create-stream-sink name *standard-output* log-format
        severity-threshold))
    (t nil)))

(defun close-log-stream (name)
  "If log sink NAME is open (see MAKE-LOG-STREAM), then this function closes the
stream and returns T. If the stream is a file stream, the file is closed too.
Otherwise, if the log sink is not open, this function does nothing."
  (let ((sink (car (remove-if-not
                     (lambda (s) (equal (getf s :name) name))
                     *logs*))))
    (when sink
      (when (getf sink :is-file)
        (close (getf sink :stream)))
      (setf *logs*
        (remove-if (lambda (s) (equal (getf s :name) name)) *logs*))
      t)))

(defun list-log-streams ()
  "Lists the names of the all the existing log sinks."
  (mapcar (lambda (s) (getf s :name)) *logs*))

(defun close-log-streams ()
  "Closes all log sinks, returning the number of log sinks that were closed.
If any stream is associated with a file, the file is closed. This function
returns the number of streams that were closed."
  (loop for name in (mapcar (lambda (s) (getf s :name)) *logs*)
    count (close-log-stream name)))

(defun plist-to-jsonl (severity plist)
  (loop for k in plist by #'cddr
    for v-raw in (cdr plist) by #'cddr
    for v = (cond
              ((and v-raw (listp v-raw) (plistp v-raw))
                (cons :map v-raw))
              ((and v-raw (listp v-raw))
                (cons :array v-raw))
              (t v-raw))
    appending (list k v) into clean-plist
    finally
    (return
      (format nil "~a~%"
        (to-json
          (ds
            `(:map
               :timestamp ,(timestamp-string)
               :severity ,severity
               :message (:map ,@clean-plist))))))))

(defun plist-to-plain (severity plist)
  (format nil "~a [~a] ~a~%"
    (timestamp-string)
    severity
    (loop for k in plist by #'cddr
      for v in (cdr plist) by #'cddr
      collect (format nil "~(~a~)=~a" k v)
      into pairs
      finally
      (return (format nil "~{~a~^; ~}" pairs)))))

(defun clean-plist (plist)
  "Removes reserved keys from PLIST."
  (loop
    for k in plist by #'cddr
    for v in (cdr plist) by #'cddr
    unless (member k *reserved-log-keys*)
    append (list k v)))

(defun plist-keys (plist)
  (sort (loop for k in plist by #'cddr collect k) #'string<))

(defun plistp (list)
  (and (evenp (length list))
       (loop for key in list by #'cddr always (keywordp key))))

(defun format-log-entry (format severity plist)
  (case format
    (:jsonl (plist-to-jsonl severity plist))
    (:plain (plist-to-plain severity plist))))

(defun plog (severity plist)
  "Outputs a log entry to every log sink (see *LOGS*) where SEVERITY is greater
than the severity of the log sink. The log entry is built using PLIST, which
consists of key/value pairs. The keys must be keywords and the values can be
strings, numbers, or simple vectors, lists, or hash-tables. For example:

    (log-it-pairs :debug :ip ip-address :port port :error error-string)

PLIST should not include :timestamp or :severity, because those are added by
this function. However, PLIST must include the :in key, with a lower-case string
naming the function that calls P-LOG. The information in PLIST is added as the
value of a :message key."
  (loop
    with sev = (getf *log-severity-map* severity)
    with sev-msg = (or sev (getf *log-severity-map* :error))
    with sev-err = (unless sev '(:in "plog" :status "invalid severity value"
                                  :severity severity))
    with bad = (unless (plistp plist)
                 `(:in "plog" :status "bad plist"
                    :plist ,(princ-to-string plist)))
    with clean-plist = (unless bad (clean-plist plist))
    with plist-err = (unless (or bad
                               (equal (plist-keys plist)
                                 (plist-keys clean-plist)))
                             `(:in "plog" :status "reserved keys ignored"
                                :keys ,(set-difference (plist-keys plist)
                                         (plist-keys clean-plist))))
    with in-err = (unless (or bad (member :in (plist-keys clean-plist)))
                    (setf clean-plist (append '(:in "(missing)") clean-plist))
                    `(:in "plog" :status "missing :in key"
                       :keys ,(plist-keys plist)))
    for log in *logs*
    for sev-log = (getf log :severity-threshold)
    for stream = (getf log :stream)
    for format = (getf log :format)
    for log-entry = (when (and (not bad) (>= sev-msg sev-log))
                      (format-log-entry format severity clean-plist))
    for plist-err-entry = (when plist-err
                            (format-log-entry format :error plist-err))
    for in-err-entry = (when in-err (format-log-entry format :error in-err))
    for bad-entry = (when bad (format-log-entry format :error bad))
    when log-entry do (format stream log-entry)
    when plist-err-entry do (format stream plist-err-entry)
    when in-err-entry do (format stream in-err-entry)
    when bad-entry do (format stream bad-entry)
    (force-output stream)
    counting log-entry))

(defun pdebug (&rest plist) (plog :debug plist))
(defun pinfo (&rest plist) (plog :info plist))
(defun pwarn (&rest plist) (plog :warn plist))
(defun perror (&rest plist) (plog :error plist))
