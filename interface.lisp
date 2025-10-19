(in-package #:interface)

(defconstant +min-width+ 80)
(defconstant +min-height+ 25)

(defun verify-size ()
  (multiple-value-bind (width height) (charms:window-dimensions charms:*standard-window*)
    (if (or (< width +min-width+) (< height +min-height+))
        (progn
          (format t "Sorry, the game was designed with at least an 80x25 terminal in mind.")
          nil)
        t)))

(defun print-words (words window)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (loop :for word :in words
          :with x = 0 :and y = 0
          :do
             (when (>= x (1- width))
               (charms:write-string-at-point "[Press any key to continue...]" window 0 (1- height))
               (charms:get-char window)
               (setf x 0)
               (setf y 0)) ; Wait until a key has been pressed to continue

             (if (< (+ x (length word)) width)
                 (progn
                   (charms:write-string-at-point word window x y)
                   (setf x (+ x (length word))))
                 (progn
                   (setf x 0)
                   (incf y))))))

(defun get-input (window)
  (let ((str ""))
    (charms/ll:mvwgetstr (charms::window-pointer window) 0 0 str)
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
