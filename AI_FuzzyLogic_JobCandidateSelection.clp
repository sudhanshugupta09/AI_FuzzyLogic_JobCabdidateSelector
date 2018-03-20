;;======================================================
;;   Job Recruitment Assistant
;;======================================================

;;------------------------------------------------------ 
; Fuzzy Variables
;;------------------------------------------------------

(defglobal ?*candidate_age* = 
    (new nrc.fuzzy.FuzzyVariable "candidate_age" 0.0 100.0 "years"))
(defglobal ?*candidate_experience* =
    (new nrc.fuzzy.FuzzyVariable "candidate_experience" 0.0 50.0 "years"))
(defglobal ?*candidate_relevantExp* =
    (new nrc.fuzzy.FuzzyVariable "candidate_relevantExp" 0.0 50.0 "years"))
(defglobal ?*Fuzzy_rating* =
    (new nrc.fuzzy.FuzzyVariable "rating" 0.0 10.0 "score"))


(defrule initial-terms
    (declare (salience 100))
=>
(import nrc.fuzzy.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)

;;------------------------------------------------------
;Primary Terms
;;------------------------------------------------------


(?*candidate_age* addTerm "low" (new ZFuzzySet 16.00 35.00))
(?*candidate_age* addTerm "medium" (new nrc.fuzzy.TriangleFuzzySet 34.00 44.00 55.00))
(?*candidate_age* addTerm "high" (new SFuzzySet 50.00 80.00)) 

(?*candidate_experience* addTerm "low" (new ZFuzzySet 0.00 6.00))
(?*candidate_experience* addTerm "medium" (new nrc.fuzzy.TriangleFuzzySet 4.00 8.00 12.00))
(?*candidate_experience* addTerm "high" (new SFuzzySet 10.00 40.00))

(?*candidate_relevantExp* addTerm "low" (new ZFuzzySet 0.00 6.00))
(?*candidate_relevantExp* addTerm "medium" (new nrc.fuzzy.TriangleFuzzySet 4.00 8.00 12.00))
(?*candidate_relevantExp* addTerm "high" (new SFuzzySet 10.00 40.00))

    
(?*Fuzzy_rating* addTerm "low" (new ZFuzzySet 3.0 5.0))
(?*Fuzzy_rating* addTerm "medium" (new PIFuzzySet 6.0 4.0))
(?*Fuzzy_rating* addTerm "high" (new SFuzzySet 7.0 10.0))
)

;;------------------------------------------------------
;Startup module
;;------------------------------------------------------

(defrule welcome-user
(declare(salience 50))
=>
(printout t "****************************************************************" crlf)
(printout t "Welcome!" crlf)
(printout t "Type the name of the candidate and press Enter> ")
(bind ?name (read))
(printout t "Let us begin the performance evaluation for " ?name "." crlf)
(printout t "Please provide the required details and" crlf)
(printout t "I will tell you the rating and recommendation for the candidate." crlf)
(printout t "****************************************************************" crlf))


;;------------------------------------------------------
;Initialization
;;------------------------------------------------------

(defrule assert-answers "initialization"
=>
    (printout t "What is the candidate's candidate_age (18-65) ? ")
    (bind ?candidate_age-value (float (readline t)))
    (printout t "How many years of total work experience does the candidate have? (1-40)")
    (bind ?candidate_experience-value (float (readline t)))
    (printout t "How many years of relevant work experience does the candidate have ? (1-40) ")   
    (bind ?candidate_relevantExp-value (float (readline t)))
    (printout t "How much would you rate the candidate in terms of interpersonal skills? (1-10)")   
    (bind ?interpersonal-skills (float (readline t)))
    (printout t "How much would you rate the candidate as a team player? (1-10)")   
    (bind ?teamplayer-value (float (readline t)))
    (printout t "How much did the candidate score on the quantitative exam? (1-10)")
    (bind ?quant-value (float (readline t)))
    (printout t "How much did the candidate score on the verbal exam? (1-10)")
    (bind ?verbal-value (float (readline t)))
    (printout t "How much did the candidate score on the interview? (1-10)")
    (bind ?interview-value (float (readline t)))


    (assert(Age_val
        (new nrc.fuzzy.FuzzyValue ?*candidate_age*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?candidate_age-value 2.0)
        ?candidate_age-value
        (+ ?candidate_age-value 2.0)))))
    (assert(Exp_val
        (new nrc.fuzzy.FuzzyValue ?*candidate_experience*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?candidate_experience-value 0.5)
        ?candidate_experience-value
        (+ ?candidate_experience-value 0.5)))))
    (assert(RelevantExp_val
        (new nrc.fuzzy.FuzzyValue ?*candidate_relevantExp*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?candidate_relevantExp-value 0.5)
        ?candidate_relevantExp-value
        (+ ?candidate_relevantExp-value 0.5)))))
    (assert(Interpersonal_val ?interpersonal-skills))
    (assert(TeamPlayer_val ?teamplayer-value))
    (assert(Quantitative-val ?quant-value))
    (assert(Verbal_val  ?verbal-value))
    (assert(Interview_val ?interview-value)))


;;------------------------------------------------------
;Fuzzy Rules for calculation
;;------------------------------------------------------



(defrule Rating_Evaluation1 ;"low candidate_age & low candidate_experience & low candidate_relevantExp=> rating very low or low"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "very low or low"))))

(defrule Rating_Evaluation2 ;"low candidate_age & high candidate_experience & low candidate_relevantExp => Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "low or medium or high"))))

(defrule Rating_Evaluation3 ;"low candidate_age & medium candidate_experience & low candidate_relevantExp => Fuzzy_rating low or medium"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "low or medium"))))

(defrule Rating_Evaluation4 ;"high candidate_age & high candidate_experience & low candidate_relevantExp=> Fuzzy_rating high or medium or low"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium or low"))))

(defrule Rating_Evaluation5 ;"high candidate_age & low candidate_experience & low candidate_relevantExp => Fuzzy_rating high or medium or low"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium or low"))))

(defrule Rating_Evaluation6 ;"high candidate_age & medium candidate_experience & low candidate_relevantExp=> Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium or low"))))

(defrule Rating_Evaluation7 ;"medium candidate_age & high candidate_experience & low candidate_relevantExp=> Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "low or medium or high"))))

(defrule Rating_Evaluation8 ;"medium candidate_age & medium candidate_experience & low candidate_relevantExp=> Fuzzy_rating low or medium"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* " low or medium "))))

(defrule Rating_Evaluation9 ;"medium candidate_age & low candidate_experience & low candidate_relevantExp=> Fuzzy_rating medium or low"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "medium or low"))))

(defrule Rating_Evaluation10 ;"low candidate_age & low candidate_experience & medium candidate_relevantExp=> Fuzzy_rating very low or low or medium"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "very low or low or medium"))))

(defrule Rating_Evaluation11 ;"low candidate_age & high candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "low or medium or high"))))

(defrule Rating_Evaluation12 ;"low candidate_age & medium candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating low or medium"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "low or medium"))))

(defrule Rating_Evaluation13 ;"high candidate_age & high candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating high or medium"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium"))))

(defrule Rating_Evaluation14 ;"high candidate_age & low candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating high or medium or low"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium or low"))))

(defrule Rating_Evaluation15 ;"high candidate_age & medium candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium or low"))))

(defrule Rating_Evaluation16 ;"medium candidate_age & high candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating medium or high"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* " medium or high"))))

(defrule Rating_Evaluation17 ;"medium candidate_age & medium candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating medium"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* " medium "))))

(defrule Rating_Evaluation18 ;"medium candidate_age & low candidate_experience  & medium candidate_relevantExp=> Fuzzy_rating medium or low"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "medium or low"))))

(defrule Rating_Evaluation19 ;"low candidate_age & low candidate_experience  & high candidate_relevantExp=> Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* " low or medium or high"))))

(defrule Rating_Evaluation20 ;"low candidate_age & high candidate_experience & high candidate_relevantExp=> Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "low or medium or high"))))

(defrule Rating_Evaluation21 ;"low candidate_age & medium candidate_experience & high candidate_relevantExp=> Fuzzy_rating low or medium or high"
    (Age_val ?a &: (fuzzy-match ?a "low"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "low or medium or high"))))

(defrule Rating_Evaluation22 ;"high candidate_age & high candidate_experience & high candidate_relevantExp=> Fuzzy_rating very high or high"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or very high"))))

(defrule Rating_Evaluation23 ;"high candidate_age & low candidate_experience & high candidate_relevantExp=> Fuzzy_rating high or medium or low"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium or low"))))

(defrule Rating_Evaluation24 ;"high candidate_age & medium candidate_experience & high candidate_relevantExp=> Fuzzy_rating medium or high"
    (Age_val ?a &: (fuzzy-match ?a "high"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "high or medium"))))

(defrule Rating_Evaluation25 ;"medium candidate_age & high candidate_experience & high candidate_relevantExp=> Fuzzy_rating medium or high"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "high"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "medium or high"))))

(defrule Rating_Evaluation26 ;"medium candidate_age & medium candidate_experience & high candidate_relevantExp=> Fuzzy_rating medium or high"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "medium"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* " medium or high "))))

(defrule Rating_Evaluation27 ;"medium candidate_age & low candidate_experience & high candidate_relevantExp=> Fuzzy_rating medium or low or high"
    (Age_val ?a &: (fuzzy-match ?a "medium"))
    (Exp_val ?e &: (fuzzy-match ?e "low"))
    (RelevantExp_val ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*Fuzzy_rating* "medium or low or high"))))


;;------------------------------------------------------
;Defuzzification
;;------------------------------------------------------

(defrule defuzzification-and-display-rating
    (declare (salience -1))
    ?f <- (theRating ?z)
    (Interpersonal_val ?p)
    (TeamPlayer_val ?t)
    (Quantitative-val ?n)
    (Verbal_val ?v)
    (Interview_val ?i)
=>
    (str-cat "Fuzzy_rating: " (?z momentDefuzzify))
    (bind ?calculated-rating (integer (+ (* 0.10 ?p) (* 0.05 ?t) (* 0.20 ?i)  (* .15 ?n) (* .15 ?v) (* .35 (?z momentDefuzzify)))))
    (if(> ?calculated-rating 10) then
    (printout t "rating : 10" crlf)
    else 
    (printout t "rating": ?calculated-rating crlf))
    (if (< ?calculated-rating 5) then
    (printout t "The candidate has poor rating and will not be a good fit for the job!" crlf)
    elif(eq ?calculated-rating 5) then
    (printout t "The candidate has a mediocre rating and may or may not be a good fit for the job!" crlf)
    elif(eq ?calculated-rating 6) then
    (printout t "The candidate has a mediocre rating and may or may not be a good fit for the job!" crlf)    
    elif(eq ?calculated-rating 7) then
    (printout t "The candidate has a good rating and will be a good fit for the job!" crlf)
    elif(eq ?calculated-rating 8) then
    (printout t "The candidate has a good rating and will be a good fit for the job!" crlf)
    elif(eq ?calculated-rating 9) then
    (printout t "The candidate has a great rating and would be a perfect fit for the job!" crlf)
    elif(eq ?calculated-rating 10) then
    (printout t "The candidate has a great rating and would be a perfect fit for the job!" crlf)
    elif(> ?calculated-rating 10) then
    (printout t "The candidate has a great rating and would be a perfect fit for the job!" crlf)
    else
    (printout t "The candidate has a low rating and is not suitable for the job!" crlf))  
    
    (halt))

;function to run the application

(deffunction run-application ()
(reset)
(run))

;Run the above function in a loop to get back the prompt every time we have to enter the values for another
;candidate or re-run the program

(while TRUE
(run-application))


