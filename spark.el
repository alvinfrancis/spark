;;--------------------------------------------------------------------
;; Spark
;;--------------------------------------------------------------------

(defvar *ticks*
  (vector ?▁ ?▂ ?▃ ?▄ ?▅ ?▆ ?▇ ?█)
  " A simple-vector of characters for representation of
sparklines.  Default is #(#\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█).

Examples:

  (defvar ternary '(-1 0 1 -1 1 0 -1 1 -1))

  (spark ternary)              => \"▁▄█▁█▄▁█▁\"

  (let ((*ticks* #(#\_ #\- #\¯)))
    (spark ternary))           => \"_-¯_¯-_¯_\"

  (let ((*ticks* #(#\▄ #\⎯ #\▀)))
    (spark ternary))           => \"▄⎯▀▄▀⎯▄▀▄\"
") 

(cl-defun spark (numbers &key min max key)
  "Generates a sparkline string for a list of real numbers.

Usage: SPARK <numbers> &key <min> <max> <key>

  * <numbers> ::= <list> of <real-number>
  * <min>     ::= { <null> | <real-number> }, default is NIL
  * <max>     ::= { <null> | <real-number> }, default is NIL
  * <key>     ::= <function>

  * <numbers> ~ data.
  * <min>    ~ lower bound of output.
               NIL means the minimum value of the data.
  * <max>    ~ upper bound of output.
               NIL means the maximum value of the data.
  * <key>    ~ function for preparing data.

Examples:

  (spark '(1 0 1 0))     => \"█▁█▁\"
  (spark '(1 0 1 0 0.5)) => \"█▁█▁▄\"
  (spark '(1 0 1 0 -1))  => \"█▄█▄▁\"

  (spark '(0 30 55 80 33 150))                 => \"▁▂▃▅▂█\"
  (spark '(0 30 55 80 33 150) :min -100)       => \"▃▄▅▆▄█\"
  (spark '(0 30 55 80 33 150) :max 50)         => \"▁▅██▅█\"
  (spark '(0 30 55 80 33 150) :min 30 :max 80) => \"▁▁▄█▁█\"

  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi (/ 1.0 4)))))
  => \"▄▆█▆▄▂▁▂▄\"
  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (cos (* x pi (/ 1.0 4)))))
  => \"█▆▄▂▁▂▄▆█\""
  (check-type numbers list)
  (check-type min     (or null real))
  (check-type max     (or null real))
  (check-type key     (or symbol function))
  (when key (setf numbers (mapcar key numbers)))

  ;; Empty data case:
  (when (null numbers)
    (cl-return-from spark ""))

  ;; Ensure min is the minimum number.
  (if (null min)
      (setf min (reduce #'min numbers))
    (setf numbers (mapcar (lambda (n) (max n min)) numbers)))

  ;; Ensure max is the maximum number.
  (if (null max)
      (setf max (reduce #'max numbers))
    (setf numbers (mapcar (lambda (n) (min n max)) numbers)))

  (when (< max min)
    (error "max %s < min %s." max min))

  (let ((unit (/ (- max min) (float (1- (length *ticks*))))))
    (when (zerop unit) (setf unit 1))
    (with-output-to-string 
      (cl-loop for n in numbers
               for nth = (floor (- n min) unit)
               do (princ (char-to-string (aref *ticks* nth)))))))

(defun generate-bar (number unit min max num-content-ticks)
  (multiple-value-bind
      (units frac) (cl-floor (- number min) (* unit num-content-ticks))
    (with-output-to-string
      (let ((most-tick (aref *vticks* num-content-ticks)))
        (dotimes (i units) (princ most-tick))
        (unless (= number max)
          ;; max number need not frac.
          ;; if number = max, then always frac = 0.
          (princ (aref *vticks* (floor frac unit))))
        (terpri)))))

(defun generate-title (title size max-lengeth-label)
  (let* ((title-string (format "%s" title))
         (mid (floor (- (if max-lengeth-label
                            (+ 1 size max-lengeth-label)
                          size)
                        (length title-string)) 2)))
    (when (plusp mid)
      (format "%s\n"
              (replace (make-string (if max-lengeth-label
                                        (+ 1 size max-lengeth-label)
                                      size)
                                    ?\s)
                       title-string :start1 mid)))))

;; TODO: find way to ensure float returned is SINGLE-FLOAT
(defun ensure-non-double-float (x)
  (if (integerp x) x (float x)))

(defun to-string (n)
  (format "%s" (ensure-non-double-float n)))

