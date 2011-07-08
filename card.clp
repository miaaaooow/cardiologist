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
(deffunction body-mass-index-ok (?mass ?height ?gender)
    "This function calculates the body mass index"
    (if  (> (/ ?mass (* ?height ?height)) 30 )
        then FALSE
        else TRUE))
     

; Heart-rate function
; 205.8 - 0.685 * age > hr
; age and hr are previously checked to be numbers

(deffunction heart-rate-ok (?heart-rate ?age) 
    (if (> (- 205.5 (* 0.685 ?age)) ?heart-rate)
        then TRUE
        else FALSE))


;Function to calculate the diagnosis on the base of a global delta 
;If the delta is less than the delta-boundary, then we cannot decide which diagnosis is right
; and go for "other" diagnosis.
(deffunction find-diagnosis (?a ?i ?h ?delta)
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
                (not (> ?isc-delts ?*delta-boundary*))
                )
            then ?*other*
            else FALSE)))



; Functions for asking questions
; Takes a question and allowed values as parameters.
; Returns a lowercase version of the user input.
(deffunction check-test-result (?test $?allowed-values) 
    (bind ?question (str-cat "What is your result on the following test: " ?test "(" ?allowed-values ")? ")) 
    (printout t ?question)
    (bind ?answer (lowcase (read))) 
    (while (and (not (lexemep ?answer)) 
                (not (member ?answer ?allowed-values))) do
            (printout t "You have entered an invalid value." crlf)
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
   
   
; DEFTEMPLATES
; Patient descriptors


; Patient current profile
(deftemplate patient-profile
	(slot age (type INTEGER) (range 1 125))
	(slot gender (type SYMBOL) (allowed-values m f))
	(slot status (type SYMBOL) (allowed-values healthy on-a-regime on-medicines supervised has-been-hospitalized hospitalized))
	(slot in-born-problem (type SYMBOL) (allowed-values yes no))
	(slot bad-hypertension-heredity (type SYMBOL) (allowed-values yes no))
)

; Patient current 
(deftemplate patient-current-indicators
    (slot weight (type FLOAT))
	(slot systolic-bp-avg (type INTEGER) (range 0 200))
	(slot diastolic-bp-avg (type INTEGER) (range 0 350))
	(slot BMI (type FLOAT) (range 0.0 70.0))
    (slot height (type FLOAT) (range 0.2 3.0))
    (slot lungs-status (type SYMBOL) (allowed-values OK problematic))
    (slot increased-kidney (type SYMBOL)(allowed-values yes no)))

(deftemplate test-type
    (slot test-name (type SYMBOL)(allowed-values blood-count blood-sugar ;
		creatine lipid-profile cardiac-history ECG increased-liver-lung ;
		cardiac-history dizziness cardiac-status))
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
		(value 0.1)))


; RULES


; Initial rule
(defrule get-the-party-started
    "Welcome message"
    =>
    (welcome)
    (watch facts))











