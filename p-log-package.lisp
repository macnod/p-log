(defpackage :p-log
  (:use :cl :dc-ds :dc-time :sb-thread)
  (:export
    close-log-stream
    close-log-streams
    list-log-streams
    make-log-stream
    pdebug
    perror
    pinfo
    plog
    pwarn))
