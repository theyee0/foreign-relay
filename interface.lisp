(in-package #:interface)

(defconstant +min-width+ 80)
(defconstant +min-height+ 25)

(defun verify-size ()
  (multiple-value-bind (width height) (charms:window-dimensions charms:*standard-window*)
    (if (or (< width +min-width+) (< height +min-height+))
        nil
        t)))

(defun print-words (words window x y)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (loop :for word :in words
          :do
             (when (>= y (1- height))
               (charms:write-string-at-point window "[Press any key to continue...]" 0 (1- height))
               (charms:get-char window) ; Wait until a key has been pressed to continue
               (charms:clear-window window)
               (setf x 0)
               (setf y 0))
             (if (< (+ x (length word) 1) width)
                 (progn
                   (charms:write-string-at-point window (concatenate 'string word " ") x y)
                   (setf x (+ x (length word) 1)))
                 (progn
                   (setf x 0)
                   (incf y)
                   (charms:write-string-at-point window (concatenate 'string word " ") x y))))
    (charms:refresh-window window)
    y))

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
