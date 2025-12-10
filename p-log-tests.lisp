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
          (is (search "in=test" s) "Log output includes :in key")
          (is (search "foo=bar" s) "Log output includes additional data"))))))

(test test-severity-filter
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "err-only" out
        :log-format :plain
        :severity-threshold :error)
      (is (= (pwarn :in "x" :msg "warn") 0))
      (is (string= "" (get-output-stream-string out)))
      (is (= (perror :in "x" :msg "boom") 1))
      (is (search "[ERROR]" (get-output-stream-string out))))))

(test test-list-and-close
  (with-clean-logs
    (let ((aout (make-string-output-stream))
           (bout (make-string-output-stream)))
      (make-log-stream "a" aout :log-format :plain)
      (make-log-stream "b" bout :log-format :plain)
      (let ((names (list-log-streams)))
        (is (member "a" names :test #'string=))
        (is (member "b" names :test #'string=)))
      (is-true (close-log-stream "a"))
      (is (not (member "a" (list-log-streams) :test #'string=)))
      (is (= 1 (close-log-streams))))))

(test test-bad-make-log-stream
  (with-clean-logs
    (is-false (make-log-stream "x" *standard-output* :log-format :xml))
    (is-false (make-log-stream "y" *standard-output* :severity-threshold :unknown))
    (is-false (make-log-stream "z" 12345))))

(test test-plog-bad-plist
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "s" out :log-format :plain)
      (let ((c (plog :info '(1 2 3))))
        (is (= c 0))
        (let ((s (get-output-stream-string out)))
          (is (search "[ERROR]" s))
          (is (search "bad plist" s)))))))

(test test-macro-bad-plist
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "s" out :log-format :plain)
      (let ((c (perror 1 2 3)))
        (is (= c 0))
        (let ((s (get-output-stream-string out)))
          (is (search "[ERROR]" s))
          (is (search "bad plist" s)))))))

(test test-missing-in-key
  (with-clean-logs
    (let ((out (make-string-output-stream)))
      (make-log-stream "s" out :log-format :plain)
      (let ((c (pinfo :foo "bar")))
        (is (= c 1))
        (let* ((s (get-output-stream-string out))
                (lines (split "\\n" s)))
          (is (search "[INFO]" (first lines)) "First line should be INFO")
          (is (search "missing" (first lines)))
          (is (search "[ERROR]" (second lines)))
          (is (search "missing :in key" (second lines))))))))

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
        (is (= c1 2))
        (is (= c2 2))
        (is (= c3 2))
        (is (= c4 3))
        (let ((s1 (get-output-stream-string out1))
              (s2 (get-output-stream-string out2))
              (s3 (get-output-stream-string out3)))
          ;; s1
          (is (search "test-1" s1))
          (is (search "test-2" s1))
          (is (search "test-3" s1))
          (is (search "test-4" s1))
          ;; s2
          (is (search "test-1" s2))
          (is (search "test-2" s2))
          (is (search "test-3" s2))
          (is (search "test-4" s2))
          ;; s3
          (is (not (search "test-1" s3)))
          (is (not (search "test-2" s3)))
          (is (not (search "test-3" s3)))
          (is (search "test-4" s3)))
        (is (every (lambda (name)
                     (member name (list-log-streams) :test #'string=))
              '("s1" "s2" "s3")))
        (is (= (length (list-log-streams)) 3))
        (is (= (close-log-streams) 3) "Closed three streams")
        (is (not (list-log-streams)))))))

;;; Run tests
(unless (run-all-tests)
  (sb-ext:quit :unix-status 1))
