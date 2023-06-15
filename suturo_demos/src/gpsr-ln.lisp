(in-package :su-demos)


;;;; list of plans
(setf list-of-plans '(:fetch :deliver :search :navigate :transport :guide :count :follow :describe :greet :nlu_fallback)) ;; 14 jn


(defun gpsr-subcribers()
	(nlplistener "NLPchatter")
	(planlistener "Planchatter")
	(hsrtospeak "hsrspeaker"))
	
	
	(defun planlistener (topic-name)
    (setf *dialog-subscriber* (roslisp:subscribe topic-name "gpsr_nlp/nlpCommands" #'subscriber-callback-function)))

;;;;;;;;;;;;;;;;;;;;;; 27 April
(defun nlplistener (topic-name)  
    (setf *plan-subscriber* (roslisp:subscribe topic-name "gpsr_nlp/nlpCommands" #'plan-callback-function)))


(defun plan-callback-function (message)
    
    (roslisp:with-fields (commands) message
         (defparameter *nlplistner-word* nil)
         
         (setf *input* commands)
         (setf *nlplistner-word* (intern (string-upcase (aref *input* 0)) :keyword))
         
          ;;(sleep 1)
         (su-real:with-hsr-process-modules
		     (when (eq *nlplistner-word* :DONE)

					(navigate-to-location :nil :start-point);;; go to the initial position
						(cram-talker "DONE") 
		     )
		     (when (eq *nlplistner-word* :START)

					(navigate-to-location :nil :start-point);;; go to the initial position
						(cram-talker "STARTING") 
						(print "GPSR starts")
		     )
		     
		     (when (eq *nlplistner-word* :FAIL)
				(cram-talker "FAIL"))
		     
		     (print *nlplistner-word*)
         )
     )
)

;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *test* nil)
(defun subscriber-callback-function (message)
    (roslisp:with-fields (commands) message
      ;(print commands)
      (setf *test* commands)
	(print *test*)
	(setf *plan* (intern (string-upcase (aref *test* 0)) :keyword))
	(setf *objectname* (intern (string-upcase (substitute #\- #\space (aref *test* 1))) :keyword))
	(setf *objecttype* (intern (string-upcase (aref *test* 2)) :keyword))
	(setf *personname* (intern (string-upcase (aref *test* 3)) :keyword))
	(setf *persontype* (intern (string-upcase (aref *test* 4)) :keyword))
	(setf *attribute* (intern (string-upcase (substitute #\- #\space (aref *test* 5))) :keyword))
	(setf *personaction* (intern (string-upcase (substitute #\- #\space (aref *test* 6))) :keyword))
	(setf *color* (intern (string-upcase (aref *test* 7)) :keyword))
	(setf *number* (intern (string-upcase (aref *test* 8)) :keyword))
	(setf *fur-location1* (intern (string-upcase (substitute #\- #\space (aref *test* 9))) :keyword))
	(setf *fur-location2* (intern (string-upcase (substitute #\- #\space (aref *test* 10))) :keyword))
	(setf *room1* (intern (string-upcase (substitute #\- #\space (aref *test* 11))) :keyword))
	(setf *room2* (intern (string-upcase (substitute #\- #\space (aref *test* 12))) :keyword))
	(cram-talker "plan")
	(dolist (?plan-type list-of-plans) ;;; find plans if it is present in the list or not (list of plans declared above)
        (when (eq ?plan-type *plan*)  ;;; TO DO Add condiation... if plan is not there in the list
          (print "plan found...")
          (sleep 1)
          
          ;;;; buffer knowledge.. 
          (when (not (eq *objectname* :it))  ;;;; for buffer knowledege of previous object
			(setf *previous-objectname* *objectname*)
		  	(setf *previous-objecttype* *objecttype*))
			
	(when (eq (check-person-pronoun *persontype*) T)
			(setf *previous-personname* *personname*)
			(setf *previous-personaction* *personaction*)
			(setf *previous-persontype* *persontype*))
		
   ;;;;; Actions
 (su-real:with-hsr-process-modules

	 		(when (eq *plan* :navigate)
			 	(print "Performing navigation ...")
				;;(setf ?output (naviagte-to-location *fur-location1* *room1*)) ;;; location-in-room or room 
				(setf ?output (navigate-to-location *fur-location1* *room1*))
				(print "Navigation Plan Done ...")
				(cram-talker ?output)
				)
	    
			 (when (eq *plan* :search)
			 	(print "Performing searching ...") ;; search for object/person on furniture/in a room (14 jn)
				(setf ?output (searching-object *objectname* *objecttype* *personname* *persontype* *personaction* *fur-location1* *room1*)) 
				(print "searching Plan Done ...")
				(cram-talker ?output)
				)
			 
			 (when (eq *plan* :fetch)
			 	(print "Performing fetching ...") 
				(setf ?output (fetching-object *objectname* *objecttype* *attribute* *fur-location1* *room1*)) ;; 14jn
				(print "Fetching Plan Done ...")
				(cram-talker ?output)
				)

			 (when (eq *plan* :deliver)
			 	(print "Performing delivering ...") ;;; deliver object to location/person
				(setf ?output (delivering-object *objectname* *fur-location1* *room1* (get-any-person-feature *personname* *persontype* *personaction*)))
				(print "Delivering Plan Done ...")
				(cram-talker ?output)
				)
				(print *previous-object*)
	    		
	    		(when (eq *plan* :transport)
			 	(print "Performing transport ...")
				(setf ?output (transporting-object *objectname* *room1* *fur-location1* *room2* *fur-location2* *personname*)) ;;; person or second location/room
				(print "Transport Plan Done ...")
				(cram-talker ?output)
				)
	    		
	    		(when (eq *plan* :guide)
			 	(print "Performing guiding ...")
				(setf ?output (guide-people *personname* *room1* *fur-location1*)) ;; room or location
				(print "Guiding Plan Done ...")
				(cram-talker ?output)
				)
			(when (eq *plan* :follow)
			 	(print "Performing following ...")
				(setf ?output (follow-people *personname* *room1* *fur-location1*)) ;; room or location
				(print "Following Plan Done ...")
				(cram-talker ?output)
				)
  	 		(when (eq *plan* :nlu_fallback)
			 	(print "No plan foud ...")
				(cram-talker "fail")
				)
		)
	 ))
	)
)






