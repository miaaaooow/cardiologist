;;; A Project in Expert Systems, AI, FMI, Sofia University
;;; Author: Maria Mateva
;;; CARDIOLOGIST - a system, simulating initial profilactics with a 
;;; 				cardiologist with a focus on hypertension problems
;;; 			
;;; Disclaimer: Please note, that this resembles reality but is NOT 
;;; medically tested or approved in any manner


;;; DEFTEMPLATES
;;; Patient descriptors

;;; Patient profile
(deftemplate patient-profile
	(slot name (type SYMBOL))
	(slot ID (type INTEGER))	
	(slot age (type INTEGER))
	(slot gender (type SYMBOL) (allowed-values m f))
	(slot status (type SYMBOL) (allowed-values healthy on-a-regime on-medicines supervised has-been-hospitalized hospitalized))
	(slot in-born-problem (type SYMBOL) (allowed-values yes no))
	(slot bad-heredity (type SYMBOL) (allowed-values yes no)))

;;; Patient current 
(deftemplate patient-current-indicators
	(slot weight (type DOUBLE) (range 0.5 400.0))
	(slot systolic-bp-avg (type INTEGER) (range 0 200))
	(slot diastolic-bp-avg (type INTEGER) (range 0 350))
	(slot BMI (type DOUBLE) (range 0 70)))
	
	
;;; DEFFACTS 
;;; People profiles
(deffacts people
	(patient-profile 
		(name Ivan Petrov)
		(ID 0004)
		(age 28)
		(gender m)
		(status healthy))
	(patient-profile 
		(name Elena Dimova)
		(ID 0104)
		(age 8)
		(gender f)
		(status healthy))		
	(patient-profile 
		(name Maria Mateva)
		(ID 0015)
		(age 23)
		(gender m)
		(status on-a-regime))
	(patient-profile 
		(name Peter Georgiev)
		(ID 0024)
		(age 82)
		(status supervised))
		(gender m)
	(patient-profile 
		(name Simona Filipova)
		(ID 0114)
		(age 77)
		(gender f)
		(status on-medicines)))
		


