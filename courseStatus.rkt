#lang racket

(require "https://api.uwaterloo.ca/v2/")  //need a vaild API key

(provide course-sections course-capacity)


;; (selector field-name alist) produces the value of the field 
;;     with name field-name in the api alist
;; selector: String (listof String Any) -> Any
(define (selector field-name alist)
  (second (first (filter (lambda (x) (equal? field-name (first x))) 
                         alist))))

;; (course-sections term subject catalog) produces a list of
;;     section names corresponding to the selected course with
;;     term, subject, and catalog
;; course-sections: Nat String Nat -> (listof String)
(define (course-sections term subject catalog)
  (local [;; (course-sec-acc term sub cata acc) produces a list of
          ;;     section names corresponding to the selected course with
          ;;     term, subject, and catalog with an accumulator acc
          ;; course-sec-acc: Nat String Nat (listof (listof String Any))
          ;;                 -> (listof String)
          (define (course-sec-acc term sub cata acc)
            (cond [(empty? acc) empty]
                  [else (cons (selector "section" (first acc))
                              (course-sec-acc term sub cata (rest acc)))]))]
         (course-sec-acc term subject catalog 
                         (uw-api (string-append "/terms/"
                                                (number->string term)
                                                "/"
                                                subject 
                                                "/"
                                                (number->string catalog)
                                                "/schedule")))))

;; (course-capacity term subject catalog) produces a list of
;;     sections and its total capacity and total enrollment
;; course-capacity: Nat String Nat -> (listof (listof String Nat))
(define (course-capacity term subject catalog)
  (local [;; (course-sec-acc term sub cata acc) produces a list of
          ;;     sections and its total capacity and total enrollment
          ;;     with an accumulator acc
          ;; course-sec-acc: Nat String Nat (listof (listof String Any))
          ;;                 -> (listof (listof String Any))
          (define (course-sec-acc term sub cata acc)
            (cond [(empty? acc) empty]
                  [else (append 
                         (list 
                          (list (selector "section" (first acc))
                                (selector "enrollment_capacity" (first acc))
                                (selector "enrollment_total" (first acc))))
                         (course-sec-acc term sub cata (rest acc)))]))]
         (course-sec-acc term subject catalog 
                         (uw-api (string-append "/terms/"
                                                (number->string term)
                                                "/"
                                                subject 
                                                "/"
                                                (number->string catalog)
                                                "/schedule")))))
