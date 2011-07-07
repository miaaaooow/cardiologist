;;; A Project in Expert Systems, AI, FMI, Sofia University
;;; Author: Maria Mateva
;;; CARDIOLOGIST - a system, simulating initial profilactics with a 
;;; 				cardiologist with a focus on hypertension problems
;;; 			
;;; Disclaimer: Please note, that this resembles reality but is NOT 
;;; medically tested or approved in any manner




;;; DEFFUNCTIONS

;;; A function to calculate BMI = body mass index
;;; Formula from http://en.wikipedia.org/wiki/Body_mass_index

;(deffunction bodymassindex (?mass ?height)
;    "This function calculates the body mass index"
;    (if (and (?numberp ?mass) (?numberp ?height))
;        then (/ ?mass (* ?height ?height))
;        else (printout t "Love is in the air!")))




;;; DEFTEMPLATES
;;; Patient descriptors

;;; Patient profile
(deftemplate patient-profile
	(slot name (type SYMBOL))
	(slot age (type INTEGER) (range 1 125))
	(slot gender (type SYMBOL) (allowed-values m f))
	(slot status (type SYMBOL) (allowed-values healthy on-a-regime on-medicines supervised has-been-hospitalized hospitalized))
	(slot in-born-problem (type SYMBOL) (allowed-values yes no))
	(slot bad-heredity (type SYMBOL) (allowed-values yes no))
)

;;; Patient current 
(deftemplate patient-current-indicators
    (slot weight (type FLOAT))
	(slot systolic-bp-avg (type INTEGER) (range 0 200))
	(slot diastolic-bp-avg (type INTEGER) (range 0 350))
	(slot BMI (type FLOAT) (range 0.0 70.0))
    (slot height (type FLOAT) (range 0.2 3.0))
    (slot lungs-status (type SYMBOL) (allowed-values OK problematic))
    (slot increased-kidney (type SYMBOL)(allowed-values yes no))
)


