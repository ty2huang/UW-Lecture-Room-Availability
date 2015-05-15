#lang racket

(require "uw-api.rkt")
(provide course-desc)
(provide online?)
(provide course-sections)
(provide course-capacity)
(provide section-info)
(provide next-holiday)
(provide room-status)

;; course-desc: String Int -> String
;;   Pre: Must be connected to the internet, and the catalog is a 3 digit number
;;   Post: The desription of the corresponding course is returned
;; Produces the description of the course given the subject and the catalog.

;; online?: String Int -> Boolean
;;   Pre: Must be connected to the internet, and the catalog is a 3 digit number
;;   Post: True
;; Produces true if course defined by subject and catalog
;; is offered online. Otherwise produces false.

;; course-sections: Int String Int -> (listof String)
;;   Pre: Must be connected to the internet, the catalog is a 3 digit
;;        number, and term is a 4 digit number
;;   Post: list of 7-character string of course sections
;; Produces the section names of a course (by subject and 
;; catalog) in a given term

;; course-capacity: Int String Int -> (listof (list String Int Int))
;;   Pre: Must be connected to the internet, the catalog is a 3 digit
;;        number, and term is a 4 digit number
;;   Post: a 7-character string for first element of each list item
;; Produces the list of section names with enrollment capacity and
;; enrollment total given term, subject, and catalog.

;; section-info: Int String Int String -> String
;;   Pre: Must be connected to the internet, the catalog is a 3 digit
;;        number, term is a 4 digit number, and section is a 7 character string
;;   Post: True
;; Produces the start time, end time, weekday(s), building, room number
;; and instructor given the term, subject, catalog, and section.

;; next-holiday: String -> String
;;   Pre: date is a 10 character string in YYYY-MM-DD
;;   Post: string that starts with a date of the holiday
;; Given a date, and provides the next holiday and its date.

;; room-status: String Int String String -> String
;;   Pre: building is non-empty string, day is a one or two character string
;;        and time is a 5 character string in 24 hour format
;;   Post: Displays course title or "FREE"
;; Gets the course that is in a building and room at the day and time. 
;; If there is no class at the time, produces "FREE".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; an APIResult is one of:
;; * (list "key" value) [where value is (union Num String)]
;; * (list "key" APIResult)               
;; * (listof APIResult)

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

(define (course-desc subject catalog)
  (find "description" (uw-api (string-append "/courses/" subject "/" 
                                             (number->string catalog)))))

(define (online? subject catalog)
  (find "online" (find "offerings" (uw-api (string-append "/courses/" subject "/" 
                                                          (number->string catalog))))))

(define (course-sections term subject catalog)
  (local
    [;; sections: APIResult -> (listof Strings)
     ;; Lists the sections of the course from nested-lst.
     (define (sections nest-lst)
       (map (lambda (x) (find "section" x)) nest-lst))]
    (sections (uw-api (string-append "/terms/" (number->string term) "/"
                                     subject "/" (number->string catalog)
                                     "/schedule")))))

(define (course-capacity term subject catalog)
  (local
    [;; capacity: APIResult -> (listof (list String Int Int))
     ;; Produces a list of lists that contain the section, enrollment
     ;; capacity, and enrollment total.
     (define (capacity nest-lst)
       (map (lambda (x) (list (find "section" x)
                              (find "enrollment_capacity" x)
                              (find "enrollment_total" x))) 
            (filter (lambda (x) (string=? (substring (find "section" x) 0 3) 
                                          "LEC")) nest-lst)))]
    (capacity (uw-api (string-append "/terms/" (number->string term) "/"
                                     subject "/" (number->string catalog)
                                     "/schedule")))))

(define (section-info term subject catalog section)
  (local
    [(define course-data 
       (uw-api (string-append "/terms/" (number->string term) "/"
                              subject "/" (number->string catalog)
                              "/schedule")))
     ;; find-section: String APIResult -> APIResult
     ;; Isolates the information from lst for the specific section.
     (define (find-section section lst)
       (cond 
         [(empty? lst) false]
         [(string=? section (find "section" (first lst))) (first lst)]
         [else (find-section section (rest lst))]))
     (define class (first (find "classes" (find-section section course-data))))
     (define class-dates (find "dates" class))
     (define class-location (find "location" class))]
    (string-append subject " " (number->string catalog) " " section " " 
                   (find "start_time" class-dates) "-" 
                   (find "end_time" class-dates) " "
                   (find "weekdays" class-dates) " "
                   (find "building" class-location) " "
                   (find "room" class-location) " "
                   (first (find "instructors" class)))))

(define (next-holiday date)
  (local
    [(define holiday-data (uw-api "/events/holidays"))
     ;; closest-coming-holiday: String APIResult APIResult -> APIResult
     ;; Produces the date and name of holiday as an association list.
     (define (closest-coming-holiday date lst acc)
       (cond 
         [(empty? lst) acc]
         [(and (not (empty? acc))
               (string<=? (find "date" (first lst)) (find "date" acc))
               (string>=? (find "date" (first lst)) date))
          (closest-coming-holiday date (rest lst) (first lst))]
         [(and (empty? acc)(string>=? (find "date" (first lst)) date))
          (closest-coming-holiday date (rest lst) (first lst))]
         [else (closest-coming-holiday date (rest lst) acc)]))
     (define holiday-date 
       (find "date" (closest-coming-holiday date holiday-data empty)))
     (define holiday-name 
       (find "name" (closest-coming-holiday date holiday-data empty)))]
    (string-append holiday-date " " holiday-name)))

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
    
     
     
     