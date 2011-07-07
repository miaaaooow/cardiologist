;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CARDIOLOGIST <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

; DEFFUNCTIONS

; Welcome message
(deffunction welcome ()
    (printout t "WECLOME to CARDIOLOGIST 1.0" crlf)
    (printout t "This is an experimental proficatic cardiologist expert system." crlf crlf)
    (printout t "For comments or suggestions, please write to me to mateva.maria in gmail" crlf))


; Body-mass index(BMI) function
(deffunction body-mass-index (?mass ?height)
    "This function calculates the body mass index"
    (if (and (numberp ?mass) (numberp ?height))
        then 
            (/ ?mass (* ?height ?height))
        else 
            (printout t "Wrong BMI params!")))


; Functions for asking questions
; Takes a question and allowed values as parameters.
; Returns a lowercase version of the user input.
(deffunction check-test-result (?test $?allowed-values) 
    (bind ?question (str-cat "What is your result on the following test: " ?test "(" ?allowed-values ")? ")) 
    (printout t ?question crlf)
    (bind ?answer (lowcase (read))) 
    (while (and (not (lexemep ?answer)) 
                (not (member ?answer ?allowed-values))) do
            (printout t "You have entered an invalid value." crlf)
            (printout t ?question crlf))
    (bind ?answer (lowcase (read)))
    ?answer)


(deffunction check-symptom (?symptom)
   (printout t "Do you lately happen to have the following symptom: ?sympom (yes/no) " ?symptom crlf) 
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
	(slot name (type SYMBOL))
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









