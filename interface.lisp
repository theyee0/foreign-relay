(in-package #:interface)

(defconstant +min-width+ 80)
(defconstant +min-height+ 25)

(defun verify-size ()
  (multiple-value-bind (width height) (charms:window-dimensions charms:*standard-window*)
    (if (or (< width +min-width+) (< height +min-height+))
        nil
        t)))

(defun print-words (window words x y)
  (if (stringp words)
      (print-words window (uiop:split-string words :separator " ") x y)
      (multiple-value-bind (width height) (charms:window-dimensions window)
        (loop :for word :in words
              :do
                 (when (>= y (1- height))
                   (charms:write-string-at-point window "[Press any key to continue...]" 0 (1- height))
                   (charms:get-char window) ; Wait until a key has been pressed to continue
                   (charms:clear-window window)
                   (setf x 0)
                   (setf y 0))
                 (when (>= (+ x (length word) 1) width)
                   (setf x 0)
                   (incf y))
                 (charms:write-string-at-point window (concatenate 'string word " ") x y)
                 (setf x (+ x (length word) 1)))
        (charms:refresh-window window)
        (1+ y))))

(defun get-input (window)
  (let ((str ""))
    (charms/ll:mvwgetstr (charms::window-pointer window) 0 0 str)
    (charms:clear-window window)
    str))

(defun make-buffer (name width height x y)
  (let ((border (charms:make-window width height x y))
        (input (charms:make-window (- width 2) (- height 2) (1+ x) (1+ y))))
    (charms/ll:box (charms::window-pointer border) 0 0)
    (charms:write-string-at-point border name 1 0)
    (charms:refresh-window border)
    (charms:refresh-window input)
    input))

(defun make-interface ()
  (multiple-value-bind (width height) (charms:window-dimensions charms:*standard-window*)
    (let ((halfwidth (floor (/ width 2)))
          (halfheight (floor (/ (- height 3) 2))))
      (values (make-buffer "Info" halfwidth (* halfheight 2) 0 0)
              (make-buffer "Letter" halfwidth halfheight halfwidth 0)
              (make-buffer "Input" halfwidth halfheight halfwidth halfheight)
              (make-buffer "Entry" (* halfwidth 2) 3 0 (* halfheight 2))))))

(defun matched-keywords (string keywords)
  (reduce #'+
          (map 'list
               (lambda (x)
                 (if (search x string) 1 0))
               keywords)))

(defun num-keywords (keywords)
  (reduce #'+
          (map 'list
               #'length
               keywords)))

(defun get-context (entry echo keywords)
  (let ((y 0)
        (line "")
        (context ""))
    (loop :while (not (equal (setf line (get-input entry)) "<start>"))
          :do
             (setf y (print-words echo (concatenate 'string "[NOT IN MESSAGE]: " line) 0 y)))
    (charms:clear-window echo)
    (loop :while (not (equal (setf line (get-input entry)) "<done>"))
          :do
             (setf y (print-words echo line 0 y))
             (setf context (concatenate 'string context line)))
    (reduce #'+
            (map 'list
                 (lambda (x) (matched-keywords context x))
                 keywords))))

(defun print-letter (message window corruption-algorithm)
  (charms:clear-window window)
  (let ((y 0))
    (setf y (print-words window "[PREFACE]" 0 y))
    (setf y (print-words window (funcall corruption-algorithm (aref message 0)) 0 y))

    (setf y (print-words window "[SALUTATION]" 0 y))
    (setf y (print-words window (funcall corruption-algorithm (aref message 1)) 0 y))

    (setf y (print-words window "[BODY]" 0 y))
    (setf y (print-words window (funcall corruption-algorithm (aref message 2)) 0 y))

    (setf y (print-words window "[CLOSING]" 0 y))
    (setf y (print-words window (funcall corruption-algorithm (aref message 3)) 0 y))))
