;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CARDIOLOGIST <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

; DEFGLOBALS

(defglobal 
    ;SYMBOLS
    ?*healthy* = "healthy"
    ?*hypertension* = "hypertension"
    ?*ischemic* = "ischemic desease"
    ?*arrhytmia* = "arrhytmia"
    ?*other* = "other"
    ;NUM CONSTS for precise calculations of diagnosis
    ?*healthiness-boundary* = 1.5
    ?*hypertension-boundary* = 1.5
    ?*arrhytmia-boundary* = 1.5
    ?*ischemic-boundary* = 1.5
    ?*delta-boundary* = 1.5
	?*tests-total-sum* = 3.0)


; DEFFUNCTIONS

; Welcome message
(deffunction welcome ()
    (printout t "WECLOME to CARDIOLOGIST 1.0" crlf)
    (printout t "This is an experimental proficatic cardiologist expert system." crlf crlf)
    (printout t "For comments or suggestions, please write to me to mateva.maria in gmail" crlf))


; Body-mass index(BMI) function
; mass and height are previously checked to be numbers
(deffunction body-mass-index-ok (?mass ?height)
    "This function calculates the body mass index"
    (/ ?mass (* ?height ?height)))
	
(deffunction is-BMI-normal (?BMI ?normal-value)
	(if (> ?normal-value ?BMI)
		then TRUE
		else FALSE))
     

; Heart-rate function
; 205.8 - 0.685 * age > hr
; age and hr are previously checked to be numbers
(deffunction is-heart-rate-ok (?heart-rate ?age) 
    (if (> (- 205.5 (* 0.685 ?age)) ?heart-rate)
        then TRUE
        else FALSE))


(deffunction is-high-blood-pressure (?age ?gender ?sbp ?dbp)
	TRUE)


;Function to calculate the diagnosis on the base of a global delta 
;If the delta is less than the delta-boundary, then we cannot decide which diagnosis is right
; and go for "other" diagnosis.
(deffunction find-diagnosis (?a ?i ?h)
    (bind ?arr-delta (- ?a ?*arrhytmia-boundary*))
    (bind ?hyp-delta (- ?h ?*hypertension-boundary*))
    (bind ?isc-delta (- ?i ?*ischemic-boundary*))
    (if (and 
            (not (> ?arr-delta 0))
            (not (> ?hyp-delta 0))
            (not (> ?isc-delta 0)))
        then ?*healthy*
        else 
            (if (and 
                (not (> ?arr-delta ?*delta-boundary*))
                (not (> ?hyp-delta ?*delta-boundary*))
                (not (> ?isc-delta ?*delta-boundary*))
                )
            then ?*other*
            else ?*healthy*)))


; Functions for asking questions
; Takes a question and allowed values as parameters.
; Returns a lowercase version of the user input.
(deffunction check-test-result (?test) 
    (bind ?question (str-cat "What is your result on the following test: " ?test "(OK/PRoblematic)? ")) 
    (printout t ?question)
    (bind ?answer (lowcase (read))) 
    (while (and (not (eq ?answer "ok")) 
                (not (eq ?answer "pr"))
				(not (eq ?answer "problematic"))) do
            (printout t "You have entered an invalid value. Please enter \"ok\" or \"pr\"" crlf)
            (printout t ?question))
    (bind ?answer (lowcase (read)))
    ?answer)


(deffunction check-symptom (?symptom)
   (bind ?question  (str-cat "Do you lately happen to have the following symptom: ?sympom (yes/no) " ?symptom)) 
   (printout t ?question)
   (bind ?answer (lowcase (read)))
   (while (not (or (eq ?answer "yes") (eq ?answer "no"))) do 
        (printout t ?question crlf)
        (printout t "Please answer with \"yes\" or \"no\"; fill in \"no\" if not sure. " crlf)
        (bind ?answer (lowcase(read))))
   if (eq ?answer "yes")
       then TRUE
       else FALSE)
   

(deffunction get-numeric-indicator (?indicator-name ?lower-bound ?upper-bound)
   (bind ?question (str-cat "Please enter your indicator of the following type: " ?indicator-name ": "))
   (printout t ?question crlf)
   (bind ?answer (lowcase (read)))
   (while (not (or (>= ?answer ?lower-bound) (<= ?answer ?upper-bound))) do 
        (printout t "Please enter a values between ?lower-bound and ?upper-bound" ?lower-bound ?upper-bound)
        (printout t ?question)
        (bind ?answer (lowcase(read))))
   ?answer)


(deffunction suggest-treatment (?diagnose ?healthy $?next)
   (printout t "CARDIOLOGIST resuls:" crlf)
   (if (eq ?healthy TRUE)
    then
        (printout t "You are completely healthy")
    else
        (printout t "You probably have some problems. CARDIOLOGIST considers ?diagnose" ?diagnose)
        (printout t crlf "You are advised to go to a specialist, preparing the following examinations:" ?next)))

   
; DEFTEMPLATES
; Patient descriptors

; Patient current profile
(deftemplate patient-profile
	(slot age (type INTEGER) (range 1 125))
	(slot gender (type INTEGER) (range 1 2))
    (slot weight (type FLOAT))
    (slot height (type INTEGER))
	;slot status (type SYMBOL) (allowed-values healthy on-a-regime on-medicines supervised has-been-hospitalized hospitalized))
	;slot in-born-problem (type SYMBOL) (allowed-values yes no))
	(slot systolic-bp-avg (type INTEGER) (range 0 200))  ; lower blood pressure bound
	(slot diastolic-bp-avg (type INTEGER) (range 0 350)) ; lower blood pressure bound
	(slot heart-rate (type INTEGER) (range 0 220))
	(slot BMI (type FLOAT) (range 0.0 70.0))
    (slot blood-count (type SYMBOL) (allowed-values OK problematic))
	(slot blood-sugar (type SYMBOL) (allowed-values OK problematic))
	(slot creatine (type SYMBOL) (allowed-values OK problematic))
	(slot lipid-profile (type SYMBOL) (allowed-values OK problematic))
	(slot cardiac-history (type SYMBOL) (allowed-values OK problematic))
	(slot cardiac-status (type SYMBOL) (allowed-values OK problematic))
	(slot ECG (type SYMBOL) (allowed-values OK problematic))
	(slot dizziness (type SYMBOL) (allowed-values OK problematic))
    (slot increased-internal-organs (type SYMBOL)(allowed-values OK problematic)))


(deftemplate test-type
    (slot test-name (type SYMBOL)(allowed-values blood-count blood-sugar ;
		creatine lipid-profile cardiac-history ECG increased-liver-lung ;
		cardiac-history dizziness cardiac-status BMI-over-limit;
        heart-rate-increased high-blood-pressure))
    (slot value (type FLOAT)))


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
	(slot add-to-sum (type FLOAT)))


; FACTS

; A matrix with empiric values of weights of which symptome proves which diagnosis.
; These might be extracted during education with an expert.

(deffacts diagnosis-symptome-weight-matrix
	(diagnosis-possibilities
		(diagnosis-name hypertension)
		(indicator shortness-of-breath)
		(value 0.7))
	(diagnosis-possibilities
		(diagnosis-name hypertension)
		(indicator fatigue)
		(value 0.2))		
	(diagnosis-possibilities
		(diagnosis-name hypertension)
		(indicator rapid-heartbeat)
		(value 0.5))
	(diagnosis-possibilities
		(diagnosis-name hypertension)
		(indicator chest-pain)
		(value 0.4))
	(diagnosis-possibilities
		(diagnosis-name hypertension)
		(indicator has-swellings)
		(value 0.2))	
	(diagnosis-possibilities
		(diagnosis-name hypertension)
		(indicator hypertonic-ancestors)
		(value 0.3))
	(diagnosis-possibilities
		(diagnosis-name ischemia)
		(indicator chest-pain)
		(value 0.3))
	(diagnosis-possibilities
		(diagnosis-name ischemia)
		(indicator has-swellings)
		(value 0.1))
	(diagnosis-possibilities
		(diagnosis-name ischemia)
		(indicator palpitations)
		(value 0.6))
	(diagnosis-possibilities
		(diagnosis-name ischemia)
		(indicator nausea)
		(value 0.9))
	(diagnosis-possibilities
		(diagnosis-name ischemia)
		(indicator sweating)
		(value 0.1))
	(diagnosis-possibilities
		(diagnosis-name ischemia)
		(indicator weakness)
		(value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia)
		(indicator shortness-of-breath)
		(value 0.4))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia)
		(indicator chest-pain)
		(value 0.3))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia)
		(indicator has-swellings)
		(value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia)
		(indicator palpitations)
		(value 0.8))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia)
		(indicator sweating)
		(value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia)
		(indicator weakness)
		(value 0.1))
	(diagnosis-possibilities
		(diagnosis-name arrhytmia)
		(indicator fainting)
		(value 0.4)))
	
	
; A matrix with empiric values of weights of each initial test.
; These might be extracted during education with an expert.
(deffacts test-type-weight-matrix
	(test-type
		(test-name blood-count)
		(value 0.3))
	(test-type
		(test-name blood-sugar)
		(value 0.4))
	(test-type
		(test-name creatine)	
		(value 0.1))
	(test-type
		(test-name lipid-profile)
		(value 0.2))
	(test-type
		(test-name cardiac-history)	
		(value 0.5))
	(test-type
		(test-name cardiac-status)
		(value 0.5))
	(test-type
		(test-name ECG)
		(value 0.5))
	(test-type
		(test-name increased-liver-lung)
		(value 0.1))
	(test-type
		(test-name dizziness)
		(value 0.1))
	(test-type
		(test-name BMI-over-limit)
		(value 0.45))
	(test-type
		(test-name heart-rate-increased)
		(value 0.5))
	(test-type
		(test-name high-blood-pressure)
		(value 0.5)))


; RULES

; Initial rule
(defrule welcome
    "Welcome message"
    =>
    (welcome)
    (watch facts))

(defrule ask-for-personal-data "patient general tests information"
	(declare (salience 100))
    =>
	(bind ?age (get-numeric-indicator "age" 1 125))
	(bind ?age (get-numeric-indicator "gender(1=f, 2=m)" 1 2))
    (bind ?height (get-numeric-indicator "height" 100 280))
    (bind ?weight (get-numeric-indicator "weight" 1 400))
	(bind ?BMI (body-mass-index-ok ?height ?weight))
    (bind ?sbp (get-numeric-indicator "systolic blood pressure avg" 0 200))
	(bind ?dbp (get-numeric-indicator "diastolic blood pressure avg" 0 350)
	(bind ?hr (get-numeric-indicator "heart rate" 0 220)
	(bind ?bc (check-test-result "blood count"))
	(bind ?bs (check-test-result "blood sugar"))
	(bind ?cr (check-test-result "creatine"))
	(bind ?lp (check-test-result "lipid profile"))
	(bind ?ch (check-test-result "cardiac history"))
	(bind ?cs (check-test-result "cardiac status"))
	(bind ?ecg (check-test-result "electro-cardiogram(ECG)"))
    (bind ?diz (check-test-result "dizziness"))
	(bind ?iio (check-test-result "increazed internal organs"))
    (if (not (is-BMI-normal ?BMI 30))
		then 
		(?value&:(test-type (test-name BMI-over-limit) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (is-high-blood-pressure (?age ?gender ?sbp ?dbp))
		then 
		(bind ?value&:(test-type (test-name heart-rate-increased) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?bc "ok"))
		then
		(bind ?value&:(test-type (test-name blood-count) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?bs "ok"))
		then
		(bind ?value&:(test-type (test-name blood-sugar) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?cr "ok"))
		then
		(bind ?value&:(test-type (test-name creatine) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?lp "ok"))
		then
		(bind ?value&:(test-type (test-name lipid-profile) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?ch "ok"))
		then
		(bind ?value&:(test-type (test-name cardiac-history) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?cs "ok"))
		then
		(bind ?value&:(test-type (test-name cardiac-status) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?ecg "ok"))
		then
		(bind ?value&:(test-type (test-name ECG) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?diz "ok"))
		then
		(bind ?value&:(test-type (test-name dizziness) (value ?value)))
		(assert (add-to-sum ?value)))
	(if (not (eq ?iio "ok"))
		then
		(bind ?value&:(test-type (test-name increazed-internal-organs) (value ?value)))
		(assert (add-to-sum ?value))))
		
    

;(defrule make-conclusions
; (declare (salience 90))
;)
;(defrule test-info
;)
;(defrule check-diagnosis )











