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
    ?*delta-boundary* = 1.5)


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
            (/ ?mass (* ?height ?height)))
     

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
(deffunction find-dagnosis (?a ?i ?h ?delta)
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
                (not (> ?arr-bound ?delta))
                (not (> ?hyp-bound ?delta))
                (not (> ?isc-bound ?delta))
                )
      then TRUE
    else FALSE))



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
   (printout t "Do you lately happen to have the following symptom: ?sympom (yes/no) " ?symptom crlf) 
   (printout t ?question)
   (bind ?answer (lowcase (read)))
   (while (not (or (eq ?answer "yes") (eq ?answer "no"))) do 
        (printout t "Do you lately happen to have the following symptom: ?symptom? Please answer with \"yes\" or \"no\"; fill in \"no\" if not sure. " ?symptom crlf)
        (bind ?answer (lowcase(read)))
   )
   if (eq ?answer "yes")
       then TRUE
       else FALSE)
   

(deffunction get-numeric-indicator (?indicator-name ?uppper-bound ?downer-bound)
   (bind ?question (str-cat "Please enter your indicator of the following type:" ?indicator-name))
   (printout t ?question crlf)
   (bind ?answer (lowcase (read)))
   (while (not (or (eq ?answer "yes") (eq ?answer "no"))) do 
        (printout t "Do you lately happen to have the following symptom: ?symptom? Please answer with \"yes\" or \"no\"; fill in \"no\" if not sure. " ?symptom crlf)
        (bind ?answer (lowcase(read)))
   )
   if (eq ?answer "yes")
       then TRUE
       else FALSE)
   
   
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
    (slot increased-kidney (type SYMBOL)(allowed-values yes no))
)

(deftemplate diagnosis-possibilities
    (slot diagnosis-name (type SYMBOL))
    (slot indicator (type SYMBOL))
    (slot value (type FLOAT) (range 0.0 1.0)))


(deftemplate diagnosis-values
    (slot siagnosis (type SYMBOL))
    (slot value (type FLOAT)))


; FACTS



; RULES


; Initial rule
(defrule get-the-party-started
    "Welcome message"
    =>
    (welcome)
    (watch facts))

; Take rule making from animals expert system

(defrule just-test
""
=>
(printout t test))









