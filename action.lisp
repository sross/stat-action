;; Copyright (c) 2008 Sean Ross
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
(in-package :sysdef)

(defclass stat-action (action) ()
  (:default-initargs :needs '(load-action)))

(defmethod component-dependencies ((component component) (action stat-action))
  nil)

(defmethod perform :before ((system system) (action stat-action) &rest args)
  (execute system 'load-action))

(defmacro aand (&rest tests)
  (if (cdr tests)
      `(let ((it ,(car tests)))
         (and it (aand ,@(cdr tests))))
      `(let ((it ,(car tests)))
         (and it))))

(defun trav (f base tree)
  (if (atom tree)
      (funcall base tree)
      (funcall f (trav f base (car tree)) (trav f base (cdr tree)))))

(defun slurp (stream &key (close t))
  (unwind-protect
      (let ((buf (make-string (file-length stream) #+lispworks :element-type #+lispworks 'lw:simple-char)))
        (read-sequence buf stream)
        buf)
    (when close (close stream))))

;; While reading in contents with #'read we make sure that if we see an :in-package
;; we bind *package* to that particular package to ensure that any #. reader macros
;; can be executed successfully.
(defun contents-of (stream &key (close t) (reader #'read))
  (let ((*package* *package*))
    (unwind-protect
        (loop for form = (funcall reader stream nil nil)
              :when (and (eql reader #'read) (and (consp form) (eql (car form) 'cl:in-package)))
              :do (setf *package* (find-package (second form))) :end
              :while form collect form)
      (when close (close stream)))))

#+lispworks
(defun open-file (file)
  (open file :external-format :utf-8 :element-type 'lw:simple-char))
#-lispworks
(defun open-file (file)
  (open file))

(defun codetree (file)
  (trav #'+ (constantly 1) (contents-of (open-file file))))

(defun codeflat (file)
  (length (alexandria:flatten (contents-of (open-file file)))))

(defun to-s (x)
  (prin1-to-string x))

(defun avg-token-len (file)
  (let ((toks (alexandria:flatten (contents-of (open-file file)))))
    (/ (reduce '+ toks :key (alexandria:compose 'length 'to-s))
       (float (length toks)))))

(defun whitespace-char-p (char)
  #+lispworks (lw:whitespace-char-p char)
  #-lispworks (find char '(#\Return #\Newline #\Space #\Tab)))

(defun codelines (file)
  (loop for line in (contents-of (open file) :reader 'read-line)
        when (aand (find-if-not 'whitespace-char-p line)
                   (not (eql it #\;)))
        sum 1))

(defun code-density (file)
  (/ (codetree file) (codelines file)))

(defun report-on (sys)
  (let ((node-sum 0)
        (elt-sum 0)
        (line-sum 0)
        (tok-len-sum 0)
        (tok-len-count 0)
        (root (component-pathname sys))
        (line (make-string 78 :initial-element #\-))
        (final-line (make-string 78 :initial-element #\=)))
    (labels ((print-headers ()
               (format t "~&~%Statistics for system ~A (~A)~%" (name-of sys) (component-pathname sys))
               (write-line line)
               (format t "~&File~31TNodes  Tokens    Lines  Density  Avg Tok Length~%")
               (write-line line))
             (process-module (module)
               (dolist (component (components-of module))
                 (cond ((typep component 'source-file)
                        (print-stats-with-restart (component-pathname component)))
                       ((typep component 'module)
                        (process-module component)))))
             (print-stats-with-restart (path)
               (restart-case (print-stats path)
                 (ignore-file () :report (lambda (stream) (format stream "Ignore path ~A." path))
                   (return-from print-stats-with-restart nil))))
             (print-stats (path)
               (incf node-sum (codetree path))
               (incf elt-sum (codeflat path))
               (incf line-sum (codelines path))
               (incf tok-len-sum (avg-token-len path))
               (incf tok-len-count)
               (format t "~30A ~6A ~9A ~5A  ~,2F~4@T ~,2F~%"
                       (enough-namestring path root)
                       (codetree path) (codeflat path)
                       (codelines path)
                       (float (code-density path))
                       (float (avg-token-len path)))))
      (print-headers)
      (process-module sys)
      (write-line line)
      (format t "~30A ~6A ~9A ~5A  ~,2F~4@T ~,2F~%"
              ""
              node-sum elt-sum line-sum
              (float (/ node-sum line-sum))
              (float (/ tok-len-sum tok-len-count)))
      (write-line final-line))))

(defmethod execute ((sys system) (action stat-action))
  (report-on sys))

;; EOF
