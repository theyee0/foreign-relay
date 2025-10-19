(in-package #:civilization)

(defstruct civilization
  (name "")
  (technology :digital)
  (relationship 0)
  (strength 0))

(defparameter +location-names+
  #("Amplicon" "Stoglich" "Dustbloom" "Jarnok" "Velanor" "Vestraix" "Velastris" "Sailyra" "Aethrone" "Lutrifen" "Celikar" "Wecineri" "Aspetref" "Nailla" "Reyhat" "Seliniox" "Xyren" "Xerakkin" "Stailfark" "Raeltarn" "Aecinad" "Strenpallasi" "Usinalex" "Uranithul" "Parasolin" "Halamex" "Thaik" "Strail" "Quellmark" "Qwedsita" "Ceqigex" "Dellamin" "Dazzonile" "Mizle" "Delaxin" "Muxin" "Milaxor")
  "A list of random, sci-fi-esque names that can be used at will")

(defun update-relationship (civilization state random-state)
  "Given whether or not your relay succeeded or failed, update your
relationship with a civilization"
  (let ((increment (if (eq state :failed) -1 1))
        (relationship (civilization-relationship civilization))
        (strength (civilization-strength civilization)))
    (setf (civilization-relationship civilization)
          (+ relationship increment))
    (setf (civilization-strength civilization)
          (+ strength (* increment strength (random 0.1 random-state))))))

(defun simulate-war (civilization-1 civilization-2 random-state)
  "Simulate a conflict between two civilizations in which strength changes"
  (let ((strength-1 (civilization-strength civilization-1))
        (strength-2 (civilization-strength civilization-2)))
    (if (< strength-1 strength-2)
        (simulate-war civilization-2 civilization-1 random-state)
        (let* ((underdog strength-2)
               (underdog-win (> underdog (random (+ underdog strength-1) random-state))))
          (setf (civilization-strength civilization-1)
                (* strength-1 (if underdog-win
                                  (- 1 (random 0.2 random-state))
                                  (+ 1 (random 0.2 random-state)))))
          (setf (civilization-strength civilization-2)
                (* strength-1 (if underdog-win
                                  (+ 1 (random 0.2 random-state))
                                  (- 1 (random 0.2 random-state)))))))))
