;;;; foreign-relay.lisp

(in-package #:foreign-relay)

(defun print-welcome-message (window)
  "Prints a welcome message with a basic guide on what the game is"
  (let ((y 0))
    (setf y (print-words window "Welcome to Foreign Relay, a game where you play the role of a relay station that intercepts letters and reformats them in non-corrupted form to pass them on. You will be responsible for identifying the keywords of the scrambled letter!" 0 0))
    (setf y (print-words window "You are able to enter things in the field at the bottom line by line. Once you receive a letter, you can start typing notes. They won't be considered until you type <start> and the program will consider all words until <end>" 0 y))
    (setf y (print-words window "Got it? Best of luck!" 0 y))
    (setf y (print-words window "Press enter to start the game..." 0 y))))

(defun random-civilization (civilization-list)
  "Selects a random civilization from a provided list"
  (aref civilization-list (random (length civilization-list))))

(defun main ()
  "Entry point for the program"
  (setf *random-state* (make-random-state t))
  (charms:with-curses ()
    (charms:refresh-window charms:*standard-window*)
    (if (verify-size)
        (multiple-value-bind (info letter echo entry) (make-interface)
          (print-welcome-message info)
          (let ((info-line 0)
                (command "")
                (civilizations (generate-civilizations *random-state*)))
            (loop :while (not (equal (setf command (get-input entry)) "<quit>"))
                  :do
                     (charms:clear-window letter)
                     (charms:clear-window echo)
                     (let ((sender (random-civilization civilizations))
                           (address (random-civilization civilizations)))
                       (setf info-line (print-words info (uiop:split-string "Decoding a letter!" :separator " ") 0 info-line))
                       (multiple-value-bind (message keywords) (generate-message sender address *random-state*)
                         (print-letter message letter
                                       (if (eq (civilization-technology sender) :digital)
                                           (lambda (x) (corruption:corrupt-string x *random-state*))
                                           (lambda (x) (corruption:corrupt-writing x 0.03 *random-state*))))
                         (let ((correct (get-context entry echo keywords))
                               (total (num-keywords keywords)))
                           (setf info-line (print-words info (format nil "You got ~A of ~A keywords in the letter." correct total) 0 info-line))
                           (if (> correct (random total *random-state*))
                               (progn
                                 (setf info-line (print-words info (format nil "The sender, ~A, is pleased with your accuracy! Your relationship with them has strengthened!" (civilization-name sender)) 0 info-line))
                                 (update-relationship sender :succeeded *random-state*))
                               (progn
                                 (setf info-line (print-words info (format nil "The sender, ~A, is displeased with your accuracy... They will reconsider before making your contact again." (civilization-name sender)) 0 info-line))
                                 (update-relationship sender :failed *random-state*)))))
                       (setf info-line (print-words info "The next letter is ready! Hit enter to continue." 0 info-line))))))
        (format t "A terminal width of at least 80x25 is required!~%")))
  (format t "Program ended. Hope to see you next time...~%"))
