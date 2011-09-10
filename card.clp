;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CARDIOLOGIST <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

; DEFGLOBALS

(defglobal 
    ;SYMBOLS
    ?*healthy* = "healthy"
    ?*hypertension* = "hypertension"
    ?*ischemia* = "ischemic disease"
    ?*arrhytmia* = "arrhytmia"
    ?*other* = "other"
    ;NUM CONSTS for precise calculations of diagnosis
    ?*healthiness-boundary* = 0.8
    ?*hypertension-boundary* = 1.5
    ?*arrhytmia-boundary* = 1.5
    ?*ischemic-boundary* = 1.5
    ?*BMI-boundary* = 30)


; DEFFUNCTIONS

; Welcome message
(deffunction welcome ()
    (printout t "WECLOME to CARDIOLOGIST 1.1" crlf)
    (printout t "This is an experimental proficatic cardiologist expert system." crlf crlf)
    (printout t "For comments or suggestions, please write to me to mateva.maria in gmail" crlf))


; Body-mass index(BMI) function
; mass and height are previously checked to be numbers
(deffunction body-mass-index (?mass ?height)
    "This function calculates the body mass index"
    (* (/ ?mass (* ?height ?height)) 10000) )
	
(deffunction is-BMI-normal (?BMI ?normal-value)
	(if (> ?normal-value ?BMI)
		then 
            TRUE
		else 
            FALSE))
     

; Heart-rate function
; 205.8 - 0.685 * age > hr
; age and hr are previously checked to be numbers
(deffunction is-heart-rate-ok (?heart-rate ?age) 
    (if (> (- 205.5 (* 0.685 ?age)) ?heart-rate)
        then 
            TRUE
        else 
            FALSE))

; Function that measures high blood pressure
; Can be much more suffisticated
(deffunction is-high-blood-pressure (?age ?gender ?sbp ?dbp)
    (if (or
           (and (eq ?gender m) (> ?age 45)  (or (> ?sbp  145) (> ?dbp 100)))
           (and (eq ?gender m) (<= ?age 45) (or (> ?sbp  135) (> ?dbp 90)))
           (and (eq ?gender f) (> ?age 45)  (or (> ?sbp  140) (> ?dbp 95)))
           (and (eq ?gender f) (<= ?age 45) (or (> ?sbp  130) (> ?dbp 85))))
        then 
            TRUE
        else 
            FALSE))


; Functions for asking questions
; Takes a question and allowed values as parameters.
; Returns a lowercase version of the user input.
(deffunction check-test-result (?test) 
    (bind ?question (str-cat "What is your result on the following test: " ?test "(OK/PRoblematic)? ")) 
    (printout t ?question)
    (bind ?answer (lowcase (read))) 
    (printout t ?answer)
    (while (and (not (eq ?answer ok)) 
                (not (eq ?answer pr))
				(not (eq ?answer problematic))) do
            (printout t "You have entered an invalid value. Please enter \"ok\" or \"pr\".: ")
            (printout t ?question)
            (bind ?answer (lowcase (read))))
    ?answer)


(deffunction check-symptom (?symptom)
    (bind ?question (str-cat "Do you lately happen to have the following symptom: " ?symptom " (Yes/No): ")) 
    (printout t ?question)
    (bind ?answer (lowcase (read)))
    (while (not (or (eq ?answer yes) (eq ?answer no) (eq ?answer y) (eq ?answer n))) do 
        (printout t ?question crlf)
        (printout t "Please answer with \"y\" or \"n\" or \"yes\" or \"no\"; fill in \"no\" if not sure.: ")
        (bind ?answer (lowcase(read))))
    (if (or (eq ?answer yes) (eq ?answer y))
        then 
            TRUE
        else   
            FALSE))
   

(deffunction get-gender ()
    (bind ?question  "Please enter your gender[f/m]: ")
    (printout t ?question)
    (bind ?answer (read))
    (while (not (or (eq ?answer m) (eq ?answer f) (eq ?answer F) (eq ?answer M))) do 
        (printout t "Please enter a value among \"f\"= female and \"m\"=male.: ")
        (printout t ?question)
        (bind ?answer (read)))
    ?answer)


(deffunction get-numeric-indicator (?indicator-name ?lower-bound ?upper-bound)
   (bind ?question (str-cat "Please enter your " ?indicator-name ": "))
   (printout t ?question)
   (bind ?answer (read))
   (while (or (not (numberp ?answer)) (< ?answer ?lower-bound) (> ?answer ?upper-bound)) do 
        (printout t "Please enter a numeric value between " ?lower-bound " and " ?upper-bound ": " crlf)
        (printout t ?question)
        (bind ?answer (read)))
   ?answer)


;; Functions to print out results
(deffunction print-init-diagnose ()
   (printout t "It seems you might have some cardiac issues." crlf)
   (printout t "Here is a list of further tests You are advised to make and then consult to Your cardiologist:" crlf)
   (printout t " - Blood Count" crlf)
   (printout t " - Blood Sugar" crlf)  
   (printout t " - Creatine" crlf)
   (printout t " - Lipid Profile" crlf))


(deffunction print-diagnose (?diagnose)
   (bind ?diag (str-cat " There is a possibility that You have " ?diagnose ". " ))
   (printout t ?diag crlf)
   (printout t "Here is a list of further tests You are advised to make in order to address this issue:" crlf)
   (if (eq ?diagnose ?*hypertension*)
	then
	   (printout t " - Creatine" crlf)
	else
	(if (eq ?diagnose ?*ischemia*)
	    then
		    (printout t " - CRD" crlf)
		    (printout t " - BET" crlf)
		    (printout t " - Echo Cardiography" crlf)
	    else
	        (if (eq ?diagnose ?*arrhytmia*)
	            then
		        (printout t " - Holter ECG" crlf)
	        	(printout t " - TSH Hormone" crlf)
		        (printout t " - Echo Cardiography" crlf)))))
  
  
; DEFTEMPLATES
; Patient descriptors

; Patient current profile
(deftemplate patient-profile
	(slot age (type INTEGER) (range 1 125))
	(slot gender (type INTEGER) (range 1 2))
    (slot weight (type FLOAT))
    (slot height (type INTEGER))
	(slot systolic-bp-avg (type INTEGER) (range 0 200))  ; lower blood pressure bound
	(slot diastolic-bp-avg (type INTEGER) (range 0 350)) ; higher blood pressure bound
	(slot heart-rate (type INTEGER) (range 0 220))
	(slot BMI (type FLOAT) (range 0.0 70.0))
	(slot cardiac-history (type SYMBOL) (allowed-values OK problematic))
	(slot cardiac-status (type SYMBOL) (allowed-values OK problematic))
	(slot ECG (type SYMBOL) (allowed-values OK problematic))
    (slot increased-liver-lung (type SYMBOL)(allowed-values OK problematic)))


(deftemplate test-type
    (slot test-name (type SYMBOL)(allowed-values cardiac-history ECG increased-liver-lung ;
		cardiac-history dizziness cardiac-status BMI-over-limit;
        heart-rate-increased high-blood-pressure))
    (slot value (type FLOAT)))


(deftemplate test-type-present
    (slot test-name (type SYMBOL)(allowed-values cardiac-history ECG increased-liver-lung ;
		cardiac-history dizziness cardiac-status BMI-over-limit;
        heart-rate-increased high-blood-pressure)))


(deftemplate symptom-present
    (slot symptom (type SYMBOL)(allowed-values shortness-of-breath fatigue;
		weakness rapid-heartbeat chest-pain has-swellings palpitations nausea;
		sweating fainting hypertonic-ancestors)))

(deftemplate diagnosis-possibilities
    (slot diagnosis-name (type SYMBOL) (allowed-values arrhytmia ischemia hypertension))
    (slot indicator (type SYMBOL) (allowed-values shortness-of-breath fatigue;
		weakness rapid-heartbeat chest-pain has-swellings palpitations nausea;
		sweating fainting hypertonic-ancestors))
    (slot value (type FLOAT) (range 0.0 1.0))) 


(deftemplate diagnosis-values
    (slot diagnosis-name (type SYMBOL))
    (slot value (type FLOAT)))


(deftemplate process-further
	(slot accum (type FLOAT)))

(deftemplate arrhytmia-accum
	(slot accum (type FLOAT)))

(deftemplate ischemia-accum
	(slot accum (type FLOAT)))

(deftemplate hypertension-accum
	(slot accum (type FLOAT)))


(deftemplate possible-disease
    (slot is-possible (type SYMBOL) (allowed-values yes no)))

(deftemplate healthy
    (slot on-diet (type SYMBOL) (allowed-values yes no)))



; FACTS

; A matrix with empiric values of weights of which symptom proves which diagnosis.
; These might be extracted during education with an expert.

(deffacts diagnosis-symptom-weight-matrix
	(diagnosis-possibilities
		(diagnosis-name hypertension) (indicator shortness-of-breath) (value 0.7))
	(diagnosis-possibilities
		(diagnosis-name hypertension) (indicator fatigue) (value 0.2))		
	(diagnosis-possibilities
		(diagnosis-name hypertension) (indicator rapid-heartbeat) (value 0.5))
	(diagnosis-possibilities
		(diagnosis-name hypertension) (indicator chest-pain) (value 0.4))
	(diagnosis-possibilities
		(diagnosis-name hypertension) (indicator has-swellings) (value 0.2))	
	(diagnosis-possibilities
		(diagnosis-name hypertension) (indicator hypertonic-ancestors) (value 0.3))
	(diagnosis-possibilities
		(diagnosis-name ischemia) (indicator chest-pain) (value 0.3))
	(diagnosis-possibilities
		(diagnosis-name ischemia) (indicator has-swellings) (value 0.1))
	(diagnosis-possibilities
		(diagnosis-name ischemia) (indicator palpitations) (value 0.6))
	(diagnosis-possibilities
		(diagnosis-name ischemia) (indicator nausea) (value 0.9))
	(diagnosis-possibilities
		(diagnosis-name ischemia) (indicator sweating) (value 0.1))
	(diagnosis-possibilities
		(diagnosis-name ischemia) (indicator weakness) (value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia) (indicator shortness-of-breath) (value 0.4))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia) (indicator chest-pain) (value 0.3))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia) (indicator has-swellings) (value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia) (indicator palpitations) (value 0.8))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia) (indicator sweating) (value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia) (indicator weakness) (value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia) (indicator fainting) (value 0.4)))
	
	
; A matrix with empiric values of weights of each initial test.
; These might be extracted during education with an expert.
(deffacts test-type-weight-matrix
	(test-type
		(test-name cardiac-history)	(value 0.5))
	(test-type
		(test-name cardiac-status) (value 0.5))
	(test-type
		(test-name ECG) (value 0.5)) 
	(test-type
		(test-name increased-liver-lung) (value 0.1))
	(test-type
		(test-name BMI-over-limit) (value 0.45))
	(test-type
		(test-name heart-rate-increased) (value 0.5))
	(test-type
		(test-name high-blood-pressure) (value 0.5)))


; RULES

; Initial welcoming rule
(defrule welcome "Welcome message"
    (declare (salience 110))
    =>
    (welcome)
   ;(watch facts))
)


(defrule ask-for-personal-data "patient general tests information"
	(declare (salience 100))
    =>
    (assert (process-further (accum 0.0)))
    (assert (arrhytmia-accum (accum 0.0)))
    (assert (ischemia-accum (accum 0.0)))
    (assert (hypertension-accum (accum 0.0)))
	(bind ?age (get-numeric-indicator "age" 1 125))
	(bind ?gender (get-gender))
    (bind ?height (get-numeric-indicator "height[cm]" 100 280))
    (bind ?weight (get-numeric-indicator "weight[kg]" 1 400))
	(bind ?BMI (body-mass-index ?weight ?height))
    (bind ?dbp (get-numeric-indicator "diastolic blood pressure(lower bound) avg" 0 200))
	(bind ?sbp (get-numeric-indicator "systolic blood pressure(upper bound) avg" 0 350))
	(bind ?hr (get-numeric-indicator "heart rate" 0 220))
	(bind ?ch (check-test-result "cardiac history"))
	(bind ?cs (check-test-result "cardiac status"))
	(bind ?ecg (check-test-result "electro-cardiogram(ECG)"))
	(bind ?ill (check-test-result "increazed liver or lung"))
    (assert (patient-profile
             (age ?age)
             (gender ?gender)
             (weight ?weight)
             (height ?height)
             (systolic-bp-avg ?sbp)
             (diastolic-bp-avg ?dbp)
             (heart-rate ?hr)
             (BMI ?BMI)
             (cardiac-history ?ch)
             (cardiac-status ?cs)
             (ECG ?ecg)
             (increased-liver-lung ?ill)))

    (if (> ?BMI ?*BMI-boundary*)
	    then 
		(assert (test-type-present (test-name BMI-over-limit)))
        (assert (healthy (on-diet yes)))
        else
        (assert (healthy (on-diet no))))
	(if (is-high-blood-pressure ?age ?gender ?sbp ?dbp)
	    then 
		(assert (test-type-present (test-name high-blood-pressure))))		 
    (if (not (is-heart-rate-ok ?hr ?age))
        then
		(assert (test-type-present (test-name heart-rate-increased))))
	(if (not (eq ?ch ok))
		then
		(assert (test-type-present (test-name cardiac-history))))		
	(if (not (eq ?cs ok))
		then
		(assert (test-type-present (test-name cardiac-status))))
	(if (not (eq ?ecg ok))
		then
		(assert (test-type-present (test-name ECG))))
	(if (not (eq ?ill ok))
		then
		(assert (test-type-present (test-name increased-liver-lung))))
    )
    

(defrule make-init-calculations
    (declare (salience 90)) 
	?tt <- (test-type-present (test-name ?name))
    (test-type (test-name ?tname&:(eq ?tname ?name)) (value ?value))
    ?f <- (process-further (accum ?sum))
    =>
    (printout t ?name)
    (retract ?f)
    (retract ?tt)
	(assert (process-further (accum (+ ?sum ?value)))))

(defrule has-diagnose
    (declare (salience 80))
    (process-further (accum ?sum))
    =>
    (if (> ?sum ?*healthiness-boundary*)
        then
            (assert (possible-disease (is-possible yes)))
        else
            (assert (possible-disease (is-possible no)))) )

(defrule healthy
    (declare (salience 70))
    (possible-disease (is-possible no))
    ?h <- (healthy (on-diet ?yn))
    =>
    (retract ?h)
    (if (eq ?yn no)
        then
        (printout t "Congratulations! Your status is: HEALTHY!" crlf)
        else
        (printout t "Congratulations! You have no symptoms for any cardiac disease." crlf)
        (printout t "Anyway, due to high BMI, we advise you to go on special diet," crlf ;
                    "in order to prevent yourself from future health problems." crlf))
    )

(defrule diagnose
    (declare (salience 70))
    (possible-disease (is-possible yes))
    =>
    (printout t "Please reply which of the following symptoms do you have" crlf)
    (if (check-symptom "shortness-of-breath")
		then
		(assert (symptom-present (symptom shortness-of-breath))))
	(if (check-symptom "fatigue")
		then
		(assert (symptom-present (symptom fatigue))))		
    (if (check-symptom "weakness")
		then
		(assert (symptom-present (symptom weakness))))
    (if (check-symptom "rapid heartbeat")
		then
		(assert (symptom-present (symptom rapid-heartbeat))))
    (if (check-symptom "chest pain")
		then
		(assert (symptom-present (symptom chest-pain))))
    (if (check-symptom "swollen limbs")
		then
		(assert (symptom-present (symptom has-swellings))))
    (if (check-symptom "palpitations")
		then
		(assert (symptom-present (symptom palpitations))))
    (if (check-symptom "nausea")
		then
		(assert (symptom-present (symptom nausea))))
    (if (check-symptom "sweating")
		then
		(assert (symptom-present (symptom sweating))))
    (if (check-symptom "fainting")
		then
		(assert (symptom-present (symptom fainting))))
	(if (check-symptom "hypertonic heritage/parents, grandparents/")
		then
		(assert (symptom-present (symptom hypertonic-ancestors)))))


; Accumulators of weights
(defrule calculate-arrhytmia-rating
	(declare (salience 60))
    (possible-disease (is-possible yes))
    ?d <- (diagnosis-possibilities (diagnosis-name arrhytmia) (indicator ?symptom) (value ?value))
	(symptom-present (symptom ?sym&:(eq ?sym ?symptom)))
	?f <- (arrhytmia-accum (accum ?accum))
	=>
    (retract ?f)
    (retract ?d)
	(assert (arrhytmia-accum (accum (+ ?accum ?value)))))

(defrule calculate-ischemia-rating
	(declare (salience 60))
    (possible-disease (is-possible yes))
	?d <- (diagnosis-possibilities (diagnosis-name ischemia) (indicator ?symptom) (value ?value))
	(symptom-present (symptom ?sym&:(eq ?sym ?symptom)))
	?f <- (ischemia-accum (accum ?accum))
	=>
    (retract ?f)
    (retract ?d)
	(assert (ischemia-accum (accum (+ ?accum ?value)))))
	
(defrule calculate-hypertension-rating
	(declare (salience 60))
    (possible-disease (is-possible yes))
	?d <- (diagnosis-possibilities (diagnosis-name hypertension) (indicator ?symptom) (value ?value))
	(symptom-present (symptom ?sym&:(eq ?sym ?symptom)))
	?f <- (hypertension-accum (accum ?accum))
	=>
    (retract ?f)
    (retract ?d)
	(assert (hypertension-accum (accum (+ ?accum ?value)))))

; Final printing rules
(defrule set-diagnose 
	(declare (salience 50))
	(possible-disease (is-possible yes))
	=>
	(print-init-diagnose))

(defrule set-arrhytmia
	(declare (salience 40))
	(arrhytmia-accum (accum ?accum&:(> ?accum  ?*arrhytmia-boundary*)))
	=>
	(print-diagnose ?*arrhytmia*))

(defrule set-ischemia
	(declare (salience 40))
	(ischemia-accum (accum ?accum&:(> ?accum  ?*ischemic-boundary*)))
	=>
	(print-diagnose ?*ischemia*))

(defrule set-hypertension
	(declare (salience 40))
	(hypertension-accum (accum ?accum&:(> ?accum  ?*hypertension-boundary*)))
	=>
	(print-diagnose ?*hypertension*))		

(defrule say-goodbye
    (declare (salience 1))
    =>
    (printout t crlf "Thanks for using Cardiologist!" crlf "We wish you good health!";
                crlf "Goodbye!" crlf))







