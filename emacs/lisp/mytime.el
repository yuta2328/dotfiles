;;; mytime.el --- Utility time functions -*- lexical-binding: t; -*-

(defun day-of-time (time) (nth 3 (decode-time time)))

(defun month-of-time (time) (nth 4 (decode-time time)))

(defun year-of-time (time) (nth 5 (decode-time time)))

(defun time-of-days (day month year) (encode-time 0 0 0 day month year))

(defun days-equal (time1 time2) (and (= (day-of-time time1) (day-of-time time2)) (= (month-of-time time1) (month-of-time time2)) (= (year-of-time time1) (year-of-time time2))))

(defun begin-of-day-of-time (time) (encode-time 0 0 0 (day-of-time time) (month-of-time time) (year-of-time time)))

(defun end-of-day-of-time (time) (encode-time 59 59 23 (day-of-time time) (month-of-time time) (year-of-time time)))

(defun day-of-week-of-time (time) (calendar-day-of-week (list (month-of-time time) (day-of-time time) (year-of-time time))))

(defun begin-of-weeks (time) (time-subtract time (days-to-time (mod (- (day-of-week-of-time time) 1) 7))))

(defun end-of-weeks (time) (time-add time (days-to-time (mod (- 7 (day-of-week-of-time time)) 7))))

(defun begin-of-month (time) (encode-time 0 0 0 1 (month-of-time time) (year-of-time time)))

(defun end-of-month (time)
  (let* ((month (month-of-time time))
         (year (year-of-time time))
         (first-of-next-month
          (if (= month 12)
              (encode-time 0 0 0 1 1 (1+ year))
		    (encode-time 0 0 0 1 (1+ month) year))))
	(time-subtract first-of-next-month (days-to-time 1))))

(provide 'mytime)
