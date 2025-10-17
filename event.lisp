(in-package #:event)

(defstruct entity
  (name "")
  (technology :digital)
  (relationship 0)
  (strength 0))

(defstruct body
  (phrases "")
  (requirements #())
  (keywords #()))

(defparameter +location-names+
  #("Amplicon" "Stoglich" "Dustbloom" "Jarnok" "Velanor" "Vestraix" "Velastris" "Sailyra" "Aethrone" "Lutrifen" "Celikar" "Wecineri" "Aspetref" "Nailla" "Reyhat" "Seliniox" "Xyren" "Xerakkin" "Stailfark" "Raeltarn" "Aecinad" "Strenpallasi" "Usinalex" "Uranithul" "Parasolin" "Halamex" "Thaik" "Strail" "Quellmark" "Qwedsita" "Ceqigex" "Dellamin" "Dazzonile" "Mizle" "Delaxin" "Muxin" "Milaxor")
  "A list of random, sci-fi-esque names that can be used at will")

(defparameter +salutation-forms+
  #("Greetings from ~A to ~A."
    "From ~A to the people of ~A:"
    "From ~A: To whom it may concern in the land of ~A:"))

(defparameter +preamble-forms+
  #.(let ((ht (make-hash-table)))
      (loop :for (key . value) :in
            `((:polite . #("Our society is doing well."
                           "We're interested in how you are doing!"
                           "We are excited to hear back from you."
                           "We appreciate your impressive relay services and encourage you to continue your good work!"
                           "We are proud to have you in our service!"
                           "Your people are among the finest citizens we have seen!"
                           "Let us visit and make merry together sometime!"
                           "We have extended our manufacturing capabilities."
                           "Unfortunately, we are too busy to be visiting soon."
                           "We would love the opportunity to see you!"
                           "You must come over to try some of our cuisine!"
                           "We commend your efforts!"
                           "The weather here has been quite reasonable."
                           "We are glad to hear that you are doing well!"
                           "We've been improving our relations with other cities."
                           "We strive to be the model of excellence!"))
              (:neutral . #("Let's get down to business."
                            "We desire a prompt response."
                            "We seek efficient and reliable communications."
                            "We find your efforts commendable, if not effective."
                            "We should find some way to improve reliability."
                            "We expect only the best from your services."
                            "Let's discuss the best way to pipeline our operations sometime."
                            "An opportunity to network our citizens would be appreciated."
                            "It would be preferred if we could inspect your operation sometime."
                            "Let us re-negotiate our contract with your relay."))
              (:rude . #("We have come to expect little from your company."
                         "It would be nice if you were less careless with your orders."
                         "We have no need to appreciate your chronic tardiness."
                         "We don't want your services."
                         "We have no wish to associate ourselves with your people."
                         "The degree to which your services flounder is laughable."
                         "We plan to replace you with our own services."
                         "We look down upon you."
                         "We expect our messages to be relayed accurately and with haste.")))
            :do (setf (gethash key ht) value))
      ht)
  "A list of conversation fragments corresponding to different relationship strengths.
Serves to add noise and volume to interactions.")

(defparameter +body-forms+
  (vector (make-body
           :phrases #("We wish to travel to ~A within the next ~A days. Please be prepared.")
           :requirements #(:location :number)
           :keywords #("travel" "days" "prepared"))
          (make-body
           :phrases #("Our relationship has not been properly managed. We wish to make amends.")
           :requirements #()
           :keywords #("relationship" "manage" "amends"))
          (make-body
           :phrases #("Our manufacturing capability has met level ~A of your standardization process.")
           :requirements #(:number)
           :keywords #("manufacturing" "capability" "level" "standard"))
          (make-body
           :phrases #("We've seen remarkable growth in our uptown area! The population has increased by nearly ~A percent!")
           :requirements #(:number)
           :keywords #("growth" "uptown" "population" "percent"))
          (make-body
           :phrases #("We have heard of troubles between ~A and ~A. We wish to request protection in case the conflict breaks into outright war.")
           :requirements #(:location :location)
           :keywords #("protection" "conflict" "war"))
          (make-body
           :phrases #("We have heard of fraud being committed... Our intelligence suspects that it is the pirates from ~A that are responsible for the sprawling issue.")
           :requirements #(:location)
           :keywords #("fraud" "suspect" "issue"))
          (make-body
           :phrases #("We wish to propose a trade of ~A units. You will receive ~A units in return. Expected profit for this transaction is ~A.")
           :requirements #(:number :number :number)
           :keywords #("trade" "return" "profit"))
          (make-body
           :phrases #("We have seen an ~A percent increase in our GDP over the past ~A years. Given this financial performance, we request to bolster our trade deals.")
           :requirements #(:number :number)
           :keywords #("increase" "GDP" "trade" "deal"))
          (make-body
           :phrases #("Our intelligence suggests that you are planning a trade deal with the merchants from ~A that sums to ~A units. We would like to make a counteroffer.")
           :requirements #(:location :number)
           :keywords #("trade" "offer"))
          (make-body
           :phrases #("We believe that you have ~A fugitives from ~A in your custody. On their behalf, we wish to take custody of the fugitives.")
           :requirements #(:number :location)
           :keywords #("fugitives" "custody"))
          (make-body
           :phrases #("We have found that the swindlers from ~A have been mistreating us. We advise you to break contact with them.")
           :requirements #(:location)
           :keywords #("mistreat" "break" "contact"))
          (make-body
           :phrases #("We plan to start an expedition in ~A days covering the trek from ~A to ~A. The goal is to foster better bonds and to build infrastructure.")
           :requirements #(:number :location :location)
           :keywords #("expedition" "bonds" "infrastructure"))))

(defparameter +signoff-forms+
  #("Regards, ~A"
    "Best, ~A"
    "Signed, ~A"))

(defun random-range (min max random-state)
  "Generate a random number between MIN and MAX"
  (+ min (random (- max min) random-state)))

(defun generate-conversation (response-list random-state)
  "Randomly select a list of strings to make a conversation"
  (let ((hash-set (make-hash-table))
        (n (random-range 2 (length response-list) random-state))
        (conversation (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop :while (< (hash-table-count hash-set) n)
          :do
             (let ((r (random (length response-list) random-state)))
               (when (not (gethash r hash-set))
                 (vector-push-extend (aref response-list r) conversation)
                 (setf (gethash r hash-set) t))))
    conversation))

(defun generate-preamble (civilization random-state)
  "Generate small talk and general discussion section of letter"
  (let ((relationship (entity-relationship civilization)))
    (generate-conversation (gethash (cond
                                      ((<= -10 relationship -5) :rude)
                                      ((< 5 relationship 5) :neutral)
                                      ((<= 5 relationship 10) :polite))
                                    +preamble-forms+)
                           random-state)))

(defun random-vector-item (v random-state)
  "Select a random item from a given vector"
  (aref v (random (length v) random-state)))

(defun generate-body-form (body-form random-state)
  "Given the form for a body, generate an actual sentence/keyword pair from the randomized list"
  (let ((phrase (random-vector-item (body-phrases body-form) random-state))
        (requirements (map 'list
                           (lambda (x)
                             (cond
                               ((eq x :number) (write-to-string (1+ (random 9 random-state))))
                               ((eq x :location) (random-vector-item +location-names+ random-state))))
                           (body-requirements body-form)))
        (keywords (body-keywords body-form)))
    `(,(apply #'format nil phrase requirements) . ,(concatenate 'vector keywords requirements))))

(defun generate-body (random-state)
  "Generate a random set of items for the body of the letter"
  (let ((hash-set (make-hash-table))
        (n (random-range 5 (length +body-forms+) random-state))
        (conversation (make-array 1 :fill-pointer 0 :adjustable t))
        (keywords (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop :while (< (hash-table-count hash-set) n)
          :do
             (let ((r (random (length +body-forms+) random-state)))
               (when (not (gethash r hash-set))
                 (setf (gethash r hash-set) t)
                 (let ((body-form (generate-body-form (aref +body-forms+ r) random-state)))
                   (vector-push-extend (car body-form) conversation)
                   (vector-push-extend (cdr body-form) keywords)))))
    `(,conversation . ,keywords)))

(defun generate-salutation (sender address random-state)
  "Generate a salutation to someone"
  (let ((n (random (length +salutation-forms+) random-state)))
    (format nil (aref +salutation-forms+ n) address) (entity-name sender)))

(defun generate-signoff (sender random-state)
  "Generate a signoff to the letter"
  (let ((n (random (length +signoff-forms+) random-state)))
    (format nil (aref +signoff-forms+ n) (entity-name sender))))
