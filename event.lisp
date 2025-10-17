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

(defparameter +salutation+
  #("Greetings, ~A"
    "To the people of ~A:"
    "To whom it may concern in the land of ~A:"))

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
  #((make-body
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
     :phrase #("We've seen remarkable growth in our uptown area! The population has increased by nearly ~A percent!")
     :requirements #(:fraction)
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
    (make-body #("We believe that you have ~A fugitives from ~A in your custody. On their behalf, we wish to take custody of the fugitives.")
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
