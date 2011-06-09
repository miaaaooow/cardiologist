;;; perform check for valid values

(deftemplate patient-profile
	(slot name (type SYMBOL))
	(slot ID (type INTEGER))	
	(slot age (type INTEGER))
	(slot gender (type SYMBOL) (allowed-values m f))
	(slot status (type SYMBOL) (allowed-values healthy on-a-regime on-medicines supervised has-been-hospitalized hospitalized)))
	

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
		


