(in-package #:corruption)

(defconstant +character-corruption-probability+ 0.1)
(defconstant +character-list+ "abcdefghijklmnopqrstuvwxyz.,!?-+_=")
(defconstant +max-dropped-sections+ 3)
(defconstant +max-scrambled-section-length+ 10)
(defconstant +max-scrambled-sections+ 5)

(defun select-subsection (left right random-state)
  "Select a random pair of numbers l, r such that left <= l <= r < right"
  (let* ((delta (- right left))
         (random-left (+ (random delta random-state) left))
         (ndelta (- right random-left))
         (random-right (+ (random ndelta random-state) random-left)))
    (list random-left random-right)))

(defun drop-section (string left right)
  "Delete the subsequence of a string on [left, right)"
  (nconc (subseq string 0 left) (nthcdr right string)))

(defun drop-sections (string random-state)
  "Delete a random number of subsequences of random length from a string"
  (labels ((drop-n-sections (string n random-state)
             (if (= n 0)
                 string
                 (drop-n-sections (apply #'drop-section string
                                         (select-subsection 0 (list-length string) random-state))
                                  (1- n)
                                  random-state))))
    (let ((dropped-sections (random +max-dropped-sections+ random-state)))
      (drop-n-sections string dropped-sections random-state))))

(defun corrupt-character (character random-state)
  "Generate a random corruption of a given character that is not equal"
  (let ((rand-char (char +character-list+ (random (length +character-list+) random-state))))
    (if (char= rand-char character)
        (corrupt-character character random-state))
        rand-char))

(defun corrupt-characters (string random-state)
  "Randomly corrupt the characters in a string"
  (if (not string)
      nil
      (let ((random-probability (random 1.0 random-state)))
        (cons (if (< random-probability +character-corruption-probability+)
                  (corrupt-character (car string) random-state)
                  (car string))
              (corrupt-characters (cdr string) random-state)))))

(defun reverse-section (string left right)
  "Reverse the subsequence of string on [left, right)"
  (nconc (subseq string 0 left)
         (reverse (subseq string left right))
         (nthcdr right string)))

(defun scramble-sections (string random-state)
  "Reverse a random number of subsequences of random length in a string to scramble its characters"
  (labels ((scramble-n-sections (string n random-state)
             (if (= n 0)
                 string
                 (scramble-n-sections (apply #'reverse-section string
                                             (select-subsection 0 (list-length string) random-state))
                                      (1- n)
                                      random-state))))
    (let ((scrambled-sections (random +max-scrambled-sections+ random-state)))
      (scramble-n-sections string scrambled-sections random-state))))

(defun corrupt-string (string random-state)
  "Given a string, return a corrupted version"
  (let* ((character-list (coerce string 'list))
         (scrambled-sections (scramble-sections character-list random-state))
         (dropped-sections (drop-sections scrambled-sections random-state))
         (corrupted-characters (corrupt-characters dropped-sections random-state)))
    (coerce corrupted-characters 'string)))
