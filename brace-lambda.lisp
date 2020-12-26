#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :com.andrewsoutar.brace-lambda
    (:documentation "Use braces for expressing lambda functions!")
  (:nicknames :com.andrewsoutar.brace-lambda/brace-lambda)
  (:use :cl)
  (:use :alexandria :anaphora :f-underscore :named-readtables)
  (:import-from :uiop #:nest)
  (:use :sb-walker)
  (:export #:brace-lambda))
(in-package :com.andrewsoutar.brace-lambda/brace-lambda)

(defun read-brace (stream char)
  (declare (ignore char))
  `(make-brace-lambda ,(package-name *package*) ,@ (read-delimited-list #\} stream t)))

(defun percent-var (package i)
  (format-symbol package "%~D" i))

(defmacro make-brace-lambda (package-name &body body &environment env)
  (declare (optimize (debug 3)))
  (let ((package (find-package package-name)))
    (nest
     (if (and (listp (car body)) (not (member (car body) '(quote function)))
              (not (endp (cdr body))))
         (destructuring-bind (lambda-list &body body) body
           (multiple-value-bind (body decls docstring)
               (parse-body body :documentation t)
             (declare (ignore docstring))
             (with-gensyms (args)
               `(lambda (&rest ,args)
                  (destructuring-bind ,lambda-list ,args
                    ,@decls
                    (block () ,@body)))))))
     (multiple-value-bind (num-of-args body)
         (if (numberp (car body))
             (values (car body) `(,(cdr body)))
             (values 0 body)))
     (let ((body (if (endp (cdr body)) (car body) body))))
     (with-gensyms (rest))
     (let* ((min-percent-var
              (loop for i from 1
                    until (not (var-lexical-p (percent-var package i) env))
                    finally (return i)))
            (max-percent-var (1- (+ min-percent-var num-of-args)))
            rest-arg
            (body
              (walk-form body env
                         (f (form context env)
                           (declare (ignore context))
                           (nest
                            (labels
                                ((make-walk-helper (orig-form thunk)
                                   (f (form context env)
                                     (declare (ignore context))
                                     (cond ((eq form orig-form)
                                            form)
                                           ((eq form (intern "%*" package))
                                            (funcall thunk form))
                                           (t
                                            (multiple-value-bind (replace convert)
                                                (inner-helper form env)
                                              (if convert
                                                  (let ((pos (position replace form)))
                                                    `(apply ',(car form)
                                                            ,@ (subseq form 1 pos)
                                                            `(,@,rest
                                                              ,,@ (subseq form (1+ pos)))))
                                                  (values replace t)))))))
                                 (inner-helper (form env)
                                   (walk-form form env
                                              (make-walk-helper
                                               form
                                               (f_ (return-from inner-helper
                                                     (values _ t))))))))
                            (let ((form (funcall
                                         (make-walk-helper (gensym) #'error)
                                         form nil env)))
                              (when (and (symbolp form)
                                         (eq package (symbol-package form))
                                         (string= "%" form :end2 1)
                                         (not (var-lexical-p form env)))
                                (if (string= "%%" form :end2 2)
                                    (unless rest-arg (setf rest-arg form))
                                    (ignore-errors
                                     (setf max-percent-var
                                           (max max-percent-var
                                                (parse-integer
                                                 (subseq (symbol-name form) 1))))))))
                            (values form (ignore-errors
                                          (eq 'make-brace-lambda (car form))))))))
            (args (loop for i from min-percent-var to max-percent-var
                        collect (percent-var package i)))))
     `(lambda (&optional ,@args &rest ,rest)
        (declare (ignorable ,@args ,rest))
        (nest
         ,@ (when rest-arg `((let ((,rest-arg ,rest)))))
         (block () ,body))))))

(defreadtable brace-lambda
  (:merge :standard)
  (:macro-char #\{ 'read-brace)
  (:macro-char #\} (f_% (error "Unmatched close brace found"))))
