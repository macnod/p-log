;; Tests for p-log package

(require :asdf)
(require :fiveam)
(require :cl-ppcre)
(require :uiop)

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :p-log)

(defpackage :p-log-tests (:use :cl :fiveam :p-log :cl-ppcre))

(in-package :p-log-tests)

(defmacro with-clean-logs (&body body)
  `(unwind-protect
     (progn
       (close-log-streams)
       ,@body)
     (close-log-streams)))

(def-suite p-log-suite :description "FiveAM tests for the p-log package")

(in-suite p-log-suite)

(test test-plain-stream-sink
  (with-clean-logs
    (let* ((out  (make-string-output-stream))
            (sink (make-log-stream "test" out
                    :log-format :plain
                    :severity-threshold :debug)))
      (is-true sink "Log sink created successfully")
      (let ((cnt (pinfo :in "test" :foo "bar")))
        (is (= cnt 1) "Wrote to single log stream")
        (let ((s (get-output-stream-string out)))
          (is (search "[INFO]" s) "Log output includes severity")
          (is (search "in=test" s) "Expected \"~a\" to contain in=test" s)
          (is (search "foo=bar" s) "Log output includes additional data"))))))

(test test-severity-filter
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "err-only" out
        :log-format :plain
        :severity-threshold :error)
      (is (= (pwarn :in "x" :msg "warn") 0) "warn should be filtered out")
      (is (string= "" (get-output-stream-string out))
        "No output for filtered warn")
      (is (= (perror :in "x" :msg "boom") 1)
        "Error should be logged")
      (is (search "[ERROR]" (get-output-stream-string out))
        "Output includes error message"))))

(test test-list-and-close
  (with-clean-logs
    (let ((aout (make-string-output-stream))
           (bout (make-string-output-stream)))
      (make-log-stream "a" aout :log-format :plain)
      (make-log-stream "b" bout :log-format :plain)
      (let ((names (list-log-streams)))
        (is (member "a" names :test #'string=) "Log stream 'a' is listed")
        (is (member "b" names :test #'string=) "Log stream 'b' is listed"))
      (is-true (close-log-stream "a") "Closed log stream 'a'")
      (is (not (member "a" (list-log-streams) :test #'string=))
        "Log stream 'a' is no longer listed")
      (is (= 1 (close-log-streams)) "Closed remaining log stream"))))

(test test-bad-make-log-stream
  (with-clean-logs
    (is-false (make-log-stream "x" *standard-output* :log-format :xml)
      "Unsupported log format")
    (is-false
      (make-log-stream "y" *standard-output* :severity-threshold :unknown)
      "Unsupported severity threshold")
    (is-false (make-log-stream "z" 12345) "Invalid file or stream")))

(test test-plog-bad-plist
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "s" out :log-format :plain)
      (let ((c (plog :info '(1 2 3))))
        (is (= c 0) "plog with bad plist should return 0")
        (let ((s (get-output-stream-string out)))
          (is (search "[ERROR]" s) "Log output includes error")
          (is (search "bad plist" s) "Log output indicates bad plist"))))))

(test test-macro-bad-plist
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "s" out :log-format :plain)
      (let ((c (perror 1 2 3)))
        (is (= c 0) "perror with bad plist should return 0")
        (let ((s (get-output-stream-string out)))
          (is (search "[ERROR]" s) "Log output includes error")
          (is (search "bad plist" s) "Log output indicates bad plist"))))))

(test test-missing-in-key
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "s" out :log-format :plain)
      (let ((c (pinfo :foo "bar")))
        (is (= c 1) "Logged despite missing :in key")
        (let* ((s (get-output-stream-string out))
                (lines (split "\\n" s)))
          (is (search "[INFO]" (first lines)) "First line should be INFO")
          (is (search "missing" (first lines))
            "First line should mention missing :in key")
          (is (search "[ERROR]" (second lines))
            "Second line should be ERROR")
          (is (search "missing :in key" (second lines))
            "Second line should mention missing :in key"))))))

(test test-multiple-streams
  (with-clean-logs
    (let ((out1 (make-string-output-stream))
           (out2 (make-string-output-stream))
           (out3 (make-string-output-stream)))
      (make-log-stream "s1" out1 :log-format :plain)
      (make-log-stream "s2" out2 :log-format :plain)
      (make-log-stream "s3" out3 :log-format :plain :severity-threshold :error)
      (let ((c1 (pdebug :in "test-1" :data "value-1"))
            (c2 (pinfo :in "test-2" :data "value-2"))
            (c3 (pwarn :in "test-3" :data "value-3"))
            (c4 (perror :in "test-4" :data "value-4")))
        (is (= c1 2) "Debug (test-1) logged to two streams")
        (is (= c2 2) "Info (test-2) logged to two streams")
        (is (= c3 2) "Warn (test-3) logged to two streams")
        (is (= c4 3) "Error (test-4) logged to three streams")
        (let ((s1 (get-output-stream-string out1))
              (s2 (get-output-stream-string out2))
              (s3 (get-output-stream-string out3)))
          ;; s1
          (is (search "test-1" s1) "Stream s1 (debug) includes test-1")
          (is (search "test-2" s1) "Stream s1 (debug) includes test-2")
          (is (search "test-3" s1) "Stream s1 (debug) includes test-3")
          (is (search "test-4" s1) "Stream s1 (debug) includes test-4")
          ;; s2
          (is (search "test-1" s2) "Stream s2 (info) includes test-1")
          (is (search "test-2" s2) "Stream s2 (info) includes test-2")
          (is (search "test-3" s2) "Stream s2 (info) includes test-3")
          (is (search "test-4" s2) "Stream s2 (info) includes test-4")
          ;; s3
          (is (not (search "test-1" s3)) "Stream s3 (error) excludes test-1")
          (is (not (search "test-2" s3)) "Stream s3 (error) excludes test-2")
          (is (not (search "test-3" s3)) "Stream s3 (error) excludes test-3")
          (is (search "test-4" s3) "Stream s3 has test-4"))
        (is (every (lambda (name)
                     (member name (list-log-streams) :test #'string=))
              '("s1" "s2" "s3"))
          "All three streams are listed")
        (is (= (length (list-log-streams)) 3) "Count of listed streams is 3")
        (is (= (close-log-streams) 3) "Closed three streams")
        (is (not (list-log-streams)) "No streams listed after close")))))

;;; Run tests
(unless (run-all-tests)
  (sb-ext:quit :unix-status 1))
