#lang racket
(require "uw-api.rkt")
(provide room-schedule)

;; room-schedule: String Int -> (listof (listof (union String Char)))
;;   PRE: must be connected to the internet, building is an existing building in UW, 
;;        and room must exist in building
;;   POST: returns a list of 6 element lists that includes the time as 5 character 
;;         string for first element and the other 5 elements are #\F for free or 
;;         #\O for occupied in the respective time for Monday, Tuesday, Wednesday, 
;;         Thursday, and Friday in that order.
;; (room-schedule building room) produces a schedule for when the building and room 
;;  is free with #\F or when it is occupied with #\O. This may be useful for finding
;;  when lecture rooms are open for studying or setting up events (or watching a movie).

; Example:
; (room-schedule "HH" 227) produces
; '(("08:30" #\F #\F #\F #\F #\F)
;   ("09:00" #\F #\F #\F #\F #\F)
;   ("09:30" #\O #\O #\O #\F #\O)
;   ("10:00" #\O #\O #\O #\O #\O)
;   ("10:30" #\O #\O #\O #\O #\O)
;   ("11:00" #\O #\O #\O #\O #\O)
;   ("11:30" #\F #\O #\O #\O #\O)
;   ("12:00" #\F #\O #\O #\F #\O)
;   ("12:30" #\F #\O #\O #\O #\O)
;   ("13:00" #\F #\O #\O #\O #\O)
;   ("13:30" #\O #\O #\O #\O #\O)
;   ("14:00" #\O #\O #\O #\O #\O)
;   ("14:30" #\O #\O #\O #\O #\O)
;   ("15:00" #\O #\O #\O #\O #\O)
;   ("15:30" #\F #\O #\F #\O #\F)
;   ("16:00" #\F #\F #\F #\F #\F)
;   ("16:30" #\F #\O #\O #\O #\O)
;   ("17:00" #\F #\O #\O #\O #\O)
;   ("17:30" #\F #\O #\O #\O #\O)
;   ("18:00" #\O #\F #\O #\F #\F)
;   ("18:30" #\O #\F #\O #\F #\F)
;   ("19:00" #\O #\F #\O #\F #\F)
;   ("19:30" #\F #\F #\F #\F #\F)
;   ("20:00" #\F #\F #\F #\F #\F)
;   ("20:30" #\F #\F #\F #\F #\F)
;   ("21:00" #\F #\F #\F #\F #\F)
;   ("21:30" #\F #\F #\F #\F #\F))

; (room-schedule "M3" 1006) produces
; '(("08:30" #\O #\O #\O #\O #\O)
;   ("09:00" #\O #\O #\O #\O #\O)
;   ("09:30" #\O #\O #\O #\O #\O)
;   ("10:00" #\O #\F #\O #\F #\O)
;   ("10:30" #\O #\O #\O #\O #\O)
;   ("11:00" #\O #\O #\O #\O #\O)
;   ("11:30" #\O #\F #\O #\F #\O)
;   ("12:00" #\O #\O #\O #\O #\O)
;   ("12:30" #\O #\O #\O #\O #\O)
;   ("13:00" #\O #\O #\O #\O #\O)
;   ("13:30" #\O #\O #\O #\O #\O)
;   ("14:00" #\O #\O #\O #\O #\O)
;   ("14:30" #\O #\O #\O #\O #\O)
;   ("15:00" #\O #\O #\O #\O #\O)
;   ("15:30" #\O #\O #\O #\O #\O)
;   ("16:00" #\O #\O #\O #\F #\O)
;   ("16:30" #\O #\O #\F #\F #\F)
;   ("17:00" #\O #\O #\F #\F #\F)
;   ("17:30" #\O #\O #\O #\O #\F)
;   ("18:00" #\O #\F #\O #\O #\F)
;   ("18:30" #\F #\O #\F #\F #\F)
;   ("19:00" #\O #\O #\O #\F #\F)
;   ("19:30" #\O #\O #\O #\F #\F)
;   ("20:00" #\O #\O #\O #\F #\F)
;   ("20:30" #\F #\O #\F #\F #\F)
;   ("21:00" #\F #\O #\F #\F #\F)
;   ("21:30" #\F #\F #\F #\F #\F)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; member?: Any (listof Any) -> Bool
;;   Pre: True
;;   Post: True
;; Determines if elem is a member of lst.
(define (member? elem lst)
  (cond
    [(empty? lst) false]
    [(equal? elem (first lst)) true]
    [else (member? elem (rest lst))]))

;; find: String APIResult -> (union APIResult Num String)
;;   Pre: lst must be an association list (it can be nested) with a string for
;;        key, and any type for value
;;   Post: True
;; Finds the value of the key called info from lst.
(define (find info lst)
  (cond
    [(empty? lst) false]
    [(string=? info (first (first lst)))
     (second (first lst))]
    [else (find info (rest lst))]))

;; room-status: String Int String String -> String
;;   Pre: building is non-empty string, day is a one or two character string
;;        and time is a 5 character string in 24 hour format
;;   Post: Displays course title or "FREE"
;; (room-status building room day time) gets the course that is in a building and room 
;; at the day and time. If there is no class at the time, produces "FREE".
(define (room-status building room day time)
  (local
    [;; split-days: String -> (listof String)
     ;; Splits str, a long string of days, into list of strings for each day.
     (define (split-days str)
       (local
         [;; split: (listof Char) -> (lisof String)
          ;; Turns each char into a string or turns #\T #\h into "Th".
          (define (split lst)
            (cond
              [(empty? lst) empty]
              [(char=? #\M (first lst)) (cons "M" (split (rest lst)))]
              [(char=? #\T (first lst)) 
               (cond
                 [(empty? (rest lst)) (cons "T" (split (rest lst)))]
                 [(char=? (first (rest lst)) #\h)
                  (cons "Th" (split (rest (rest lst))))]
                 [else (cons "T" (split (rest lst)))])]
              [(char=? #\W (first lst)) (cons "W" (split (rest lst)))]
              [else (cons "F" (split (rest lst)))]))]
         (split (string->list str))))
     (define room-data (uw-api (string-append "/buildings/" building 
                                              "/" (number->string room)
                                              "/courses")))
     ;; room-state: APIResult -> String
     ;; Produces title of the course as a string or "FREE".
     (define (room-state lst)
       (cond
         [(empty? lst) "FREE"]
         [(and (member? day (split-days (find "weekdays" (first lst))))
               (string<=? (find "start_time" (first lst)) time 
                          (find "end_time" (first lst))))
          (display-course (first lst))]
         [else (room-state (rest lst))]))
     ;; display-course: APIResult -> String
     ;; Takes a list for the course at the specified time and string-append
     ;; to the appropriate result.
     (define (display-course lst)
       (string-append (find "subject" lst) " " (find "catalog_number" lst)
                      " " (find "title" lst)))]
    (room-state room-data)))


(define (room-schedule building room)
  (local
    [(define template '(("08:30")("09:00")("09:30")("10:00")("10:30")("11:00")
                        ("11:30")("12:00")("12:30")("13:00")("13:30")("14:00")
                        ("14:30")("15:00")("15:30")("16:00")("16:30")("17:00")
                        ("17:30")("18:00")("18:30")("19:00")("19:30")("20:00")
                        ("20:30")("21:00")("21:30")))
     ;; fix-schedule: (listof (list String)) -> (listof (listof (union String Char)))
     ;;   PRE: the string in schedule is the time as a 5 character string
     ;;   POST: returns the schedule with 5 #\F or #\O added to each list
     ;; (fix-schedule schedule) adjusts each element lists of the entire list.
     (define (fix-schedule schedule)
       (if (empty? schedule) empty 
           (cons (fix-times (first schedule)) 
                 (fix-schedule (rest schedule)))))
     ;; fix-times: (list String) -> (listof (union String Char))
     ;;   PRE: the string in time-slot is the time as a 5 character string
     ;;   POST: returns a 6 element list that includes the time as 5 character 
     ;;         string for first element and the other 5 elements are #\F for free or 
     ;;         #\O for occupied in the respective time for Monday, Tuesday, Wednesday, 
     ;;         Thursday, and Friday in that order.
     ;; (fix-times time-slot) adds the occupancy to the list.
     (define (fix-times time-slot)
       (cons (first time-slot) 
             (fix-day (first time-slot) "M")))
     ;; fix-day: String String -> (listof Char)
     ;;   PRE: time is a 5 character string and is a 1 or 2 character string
     ;;   POST: returns a list of #\F or #\O
     ;; (fix-day time day) produces a list of 5 elements for each Monday,
     ;;  Tuesday, Wednesday, Thursday, and Friday
     (define (fix-day time day)
       (cond
         [(string=? "M" day) (cons (f/o time "M") (fix-day time "T"))]
         [(string=? "T" day) (cons (f/o time "T") (fix-day time "W"))]
         [(string=? "W" day) (cons (f/o time "W") (fix-day time "Th"))]
         [(string=? "Th" day) (cons (f/o time "Th") (fix-day time "F"))]
         [(string=? "F" day) (cons (f/o time "F") empty)]))
     ;; f/o: String String -> Char
     ;;   PRE: time is a 5 character string and is a 1 or 2 character string
     ;;   POST: returns either #\F or #\O
     ;; (f/o time day) produces #\F when room is free or #\O when occupied.
     (define (f/o time day)
       (if (string=? (room-status building room day time) "FREE") #\F #\O))]
    (fix-schedule template)))
           
         
    