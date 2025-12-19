(defpackage :p-log
  (:use :cl :sb-thread)
  (:local-nicknames
    (:re :ppcre)
    (:ds :dc-ds)
    (:dt :dc-time))
  (:export
    close-log-stream
    close-log-streams
    list-log-streams
    make-log-stream
    pdebug
    perror
    pinfo
    plog
    pwarn
    pversion
    ))
