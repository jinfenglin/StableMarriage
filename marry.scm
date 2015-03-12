;; This is the code for -- Stable Marriage
;;@author Jinfeng Lin (ljf1992@brandeis.edu)
;;Assignment: PS03 -- Marry

#lang scheme
(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers proposees)
  (zip-together (send proposers 'name)
                (send (send proposers 'intended) 'name)))

(define (courtship unengaged-proposers proposers proposees) ;let proposers try to propose to proposees and itera this process until the unengaged list to be '()
   (send unengaged-proposers 'propose) ;let all the proposer begin propose
   (cond ((null? unengaged-proposers) '(end)); end the iterlation when everyone is engaged
         (else (courtship (currently-unengaged proposers)  proposers proposees)) ;begin the next iteration, update the unengaged subset
   )
  )

; lambda inside is a procedure which judge a single person, if it is engaged return true else return false
; This prediction will be utilized by filter function, which go thorugh the list and filter all the element don't statisfy 
; the predict.
(define (currently-unengaged list-of-people) 
  (filter (
           lambda(person)
           (cond ((null? (person 'intended)) #t)
                 (else #f)
           )) list-of-people)
  )

;transfer message every member inside the list. And chain the reaction of each memeber together as a list
(define (send list-of-people message)
  (if (pair? list-of-people) 
      (cons ((car list-of-people) message) (send (cdr list-of-people) message))
      '()
 ))

(define (couple? person1 person2) 
  (cond ((and (eq? (person1 'intended) person2) (eq? (person2 'intended) person1)) #t)
        (else #f)
  )); if the current-intended of p1 and p2 are each other, then it is a couple return true else return false.

(define (zip-together list1 list2)
  (if (null? list1)
      '()
      (cons (list (car list1) (car list2))
            (zip-together (cdr list1) (cdr list2)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (love-more person1 person2);return the first it found in the preference list, because preference list is ordered.
  (lambda (preference-list)
    (cond ((eq? (car preference-list) person1) person1) ;if find person 1 first, return person1
          ((eq? (car preference-list) person2) person2) ;if find person 2 first, return person2
          (else ((love-more person1 person2) (cdr preference-list)) ) ; if don't match, search for the next one.
    )
  ))
(define (make-person my-name)
  (let ((preference-list '())
        (possible-mates '())
        (current-intended '()))
    (define (me message)
      (cond ((eq? message 'name) my-name)
            ((eq? message 'intended) current-intended)
            ((eq? message 'loves) preference-list)
            ((eq? message 'possible) possible-mates)
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
            
            ((eq? message 'propose)
               (let ((beloved (car possible-mates)))
                 (set! possible-mates (cdr possible-mates))
                 (write-line (list (me 'name) "say 'I love you' to" (beloved 'name) ))
                 (if (eq? ((beloved 'i-love-you) me);current intendent store a procedure (people) not a name
                          'i-love-you-too)
                     (begin (set! current-intended beloved)
                            (write-line (list (beloved 'name) "say 'I love you too' to" (me 'name)))
                            (write-line (list (beloved 'name) "are engaged with" (me 'name)))
                            (write-line "")
                            'we-are-engaged)
                     (begin (write-line (list (beloved 'name) (list "say 'sorry" (me 'name) ",you are cute, but I love" ((beloved 'intended) 'name)"'" )))
                            (write-line "")
                            'no-one-loves-me))))
            ((eq? message 'i-love-you) 
             (lambda (to-be-considered)
               ;if Mr to-be-considered have highest pripority, else send back I love you too otherwise; if to-be-consider outcompete the former Mr right, use i-change-my-mind to kick him out
               (cond ((null? current-intended) 
                      (set! current-intended to-be-considered)
                      'i-love-you-too) ;if current-intendent is null then accept proposel
                     ((eq? ((love-more current-intended to-be-considered) preference-list) current-intended) 'buzz-off-creep);love current one more,say sorry to to-be-considered..we will hold your resume bala bala every company say something like that, I hate it :(
                     (else ((current-intended 'i-changed-my-mind) me)
                           (set! current-intended to-be-considered)
                           (write-line (list (me 'name) "say 'I love you too' to" (to-be-considered 'name)))
                            (write-line (list (me 'name) "are engaged with" (to-be-considered 'name)))
                            (write-line "")
                           'i-love-you-too
                           );dump the current one
                )
                 )
             );it should output i-love-you-too or buzz-off-creep. it reveive a name (me) as parameter, to see if the beloved love me as well
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                            (set! current-intended '())
                            (write-line (list (me 'name) "cried,'" (lost-love 'name) "dumped me!'"))
                            
                            'dumped!)
                        (else 
                            'there-must-be-some-misunderstanding))))
            (else 
              (error "Bad message to a person " (list me my-name message)))))
      me))
           
(define (write-line x)
  (display x)
  (newline))

;; This is a test file for -- Stable Marriage

(define alan (make-person 'Alan))
(define bob (make-person 'Bob))
(define charles (make-person 'Chuck))
(define david (make-person 'Dave))
(define ernest (make-person 'Ernie))
(define franklin (make-person 'Frank))
(define agnes (make-person 'Agnes))
(define bertha (make-person 'Bertha))
(define carol (make-person 'Carol))
(define deborah (make-person 'Debbie))
(define ellen (make-person 'Ellen))
(define francine (make-person 'Fran))

((alan 'load-preferences) 
   (list agnes carol francine bertha deborah ellen))
((bob 'load-preferences) 
   (list carol francine bertha deborah agnes ellen))
((charles 'load-preferences) 
   (list agnes francine carol deborah bertha ellen))
((david 'load-preferences) 
   (list francine ellen deborah agnes carol bertha))
((ernest 'load-preferences) 
   (list ellen carol francine agnes deborah bertha))
((franklin 'load-preferences) 
   (list ellen carol francine bertha agnes deborah))
((agnes 'load-preferences) 
   (list charles alan bob david ernest franklin))
((bertha 'load-preferences) 
   (list charles alan bob david ernest franklin))
((carol 'load-preferences) 
   (list franklin charles bob alan ernest david))
((deborah 'load-preferences) 
   (list bob alan charles franklin david ernest))
((ellen 'load-preferences) 
   (list franklin charles bob alan ernest david))
((francine 'load-preferences) 
   (list alan bob charles david franklin ernest))

(define men (list alan bob charles david ernest franklin))
(define women (list agnes bertha carol deborah ellen francine))



(send men 'name)
(send women 'name)

(match-make men women)





