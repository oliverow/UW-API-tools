;; (selector field-name alist) produces the value of the field 
;;     with name field-name in the api alist
;; selector: String (listof String Any) -> Any
(define (selector field-name alist)
  (second (first (filter (lambda (x) (equal? field-name (first x))) 
                         alist))))

;; (goose-nest-around latitude longitude dist) provides a list of 
;;     locations of all goose nests around within distance dist (in meters)
;;     consuming latitude (North is positive) 
;;     and longitude (West is negative)
;; goose-nest-around: Nat Nat Nat-> (listof String)
(define (goose-nest-around latitude longitude dist)
  (local [(define real-dist (/ dist 100000))
          (define api-list (uw-api "/resources/goosewatch"))
          (define (gn-around-acc latitude longitude acc)
            (cond [(empty? acc) empty]
                  [(and 
                    (<
                     (abs (- latitude (selector "latitude" (first acc))))
                     real-dist)
                    (<
                     (abs (- longitude (selector "longitude" (first acc))))
                     real-dist))
                   (cons (selector "location" (first acc))
                         (gn-around-acc latitude longitude (rest acc)))]
                  [else (gn-around-acc latitude longitude (rest acc))]))]
         (cond [(empty? (gn-around-acc latitude longitude api-list))
                (list "No Goose Nest Around")]
               [else (gn-around-acc latitude longitude api-list)])))
