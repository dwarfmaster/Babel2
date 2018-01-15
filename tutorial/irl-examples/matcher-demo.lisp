
(asdf:operate 'asdf:load-op :irl)

(in-package :irl)

(activate-monitor trace-irl-in-web-browser)

;; Flexible interpretation
;; Not blindly execute the FCG output as an IRL program
;; But, find a network that matches the FCG output that can be evaluated

;; How matching works
;; Matching is not the same thing as unification

;; 1 matching is needed for flexible interpretation
;; 2 the output of FCG is often incomplete
;; 3 it is matched against chunks

(defclass class-prototype (entity) ())

(defclass color-prototype (entity) ())

(setf *chunk1*
      (make-instance 
       'chunk 
       :irl-program '((get-context ?ctx)
                      (filter-by-class ?set-1 ?ctx ?class)
                      (filter-by-color ?set-2 ?set-1 ?color)
                      (unique-entity ?target ?set-2))
       :target-var '(?target . entity)
       :open-vars '((?class . class-prototype) (?color . color-prototype))))


;; 1 All the primitves in the meaning must be in the chunk (not the other way around)
;; 2 All the bind statements in the meaning must match an open variable (or a bind statement in the chunk)
;; 3 All the links in the meaning must be in the chunk (chunks can add links meanings not)

;; Perfect Match
(clear-page)      

(match-chunk *chunk1* '((get-context ?ctx)
                        (filter-by-class ?set-1 ?ctx ?class)
                        (filter-by-color ?set-2 ?set-1 ?color)
                        (unique-entity ?target ?set-2)
                        (bind class-prototype ?class block)
                        (bind color-prototype ?color red)))


;; Only bind statements (with typing)
(clear-page)

(match-chunk *chunk1* '((bind class-prototype ?class block)
                        (bind color-prototype ?color red)))


;; Only bind statements (without typing)

(clear-page)

(match-chunk *chunk1* '((bind entity ?class block)
                        (bind entity ?color red)))

;; All the primitives in the meaning should match one in the chunk

(clear-page)

(match-chunk *chunk1* '((get-context ?ctx)
                        (filter-by-class ?set-1 ?ctx ?class)
                        (filter-by-bla ?set-2 ?set-1 ?color)
                        (unique-entity ?target ?set-2)
                        (bind class-prototype ?class block)
                        (bind color-prototype ?color red)))

;; The meaning can contain less primitives

(clear-page)

(match-chunk *chunk1* '((get-context ?ctx)
                        (filter-by-class ?set-1 ?ctx ?class)
                        (unique-entity ?target ?set-2)
                        (bind class-prototype ?class block)
                        (bind color-prototype ?color red)))

;; Chunks can add a link to the result 

(clear-page) 

(match-chunk *chunk1* '((get-context ?ctx)
                        (filter-by-class ?set-1 ?ctx ?class)
                        (filter-by-color ?set-2 ?set-1 ?color)
                        (unique-entity ?target ?set-3)
                        (bind class-prototype ?class block)
                        (bind color-prototype ?color red)))

;; Even if the meaning is completely disconnected

(clear-page)

(match-chunk *chunk1* '((get-context ?a)
                        (filter-by-class ?c ?b ?foo)
                        (filter-by-color ?e ?d ?bar)
                        (unique-entity ?g ?f)
                        (bind class-prototype ?class block)
                        (bind color-prototype ?color red)))

;; But aditional links in the meaning can not overrule the chunk

(clear-page)

(match-chunk *chunk1* '((get-context ?ctx)
                        (filter-by-class ?set-1 ?ctx ?class)
                        (filter-by-color ?set-2 ?set-1 ?color)
                        (unique-entity ?ctx ?set-2)
                        (bind class-prototype ?class block)
                        (bind color-prototype ?color red)))

