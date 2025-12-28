(defun parse-and-eval (expr)
  (if (atom expr)
      expr
      (let ((op (car expr))
            (args (mapcar #'parse-and-eval (cdr expr))))
        (case op
          (+ (apply #'+ args))
          (- (apply #'- args))
          (* (apply #'* args))
          (/ (apply #'/ args))
          (t (error "Unknown operator ~a" op))))))

(defun read-exprs-from-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil nil)
          while line
          when (string-trim " " line) ; пропускаем пустые строки
          collect (read-from-string line))))

(defun main (args)
  (let ((input "input.txt")
        (output "output.txt"))
    (let ((i-position (position "-i" args :test #'string=)))
      (when i-position
        (when (< (1+ i-position) (length args))
          (setf input (nth (1+ i-position) args)))))
    (let ((o-position (position "-o" args :test #'string=)))
      (when o-position
        (when (< (1+ o-position) (length args))
          (setf output (nth (1+ o-position) args)))))

    (let ((exprs (read-exprs-from-file input)))
      (let ((results (mapcar #'parse-and-eval exprs)))
        (with-open-file (out output :direction :output :if-exists :supersede)
          (dolist (res results)
            (format out "~a~%" res)))
        (format t "Вычисление завершено. Результаты в ~a~%" output)))))

(main sb-ext:*posix-argv*)