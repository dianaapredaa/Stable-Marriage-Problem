#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.


(define (get-unstable-couples engagements mpref wpref)
  (let loop ((engs engagements)
             (acc '()))
    ; iterate through engagements to find unstable one's
    (if (null? engs)
        ; return unstable matches
        acc
        (let* ((couple (car engs)) (woman (car couple)) (man (cdr couple)))
          ; check if the match is stable or not
          (if (or (better-match-exists? man woman (get-pref-list mpref man) wpref engagements)
                   (better-match-exists? woman man (get-pref-list wpref woman) mpref (map (lambda (x) (cons (cdr x) (car x))) engagements)))
              ; add ustable match to the unstable matches lis
              (loop (cdr engs) (cons couple acc))
              (loop (cdr engs) acc))))))

; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  ; iterate through unmarried man to find new marriages
  (let men-loop ((free-m free-men)
                 (engs engagements))
    (if (null? free-m)
        ; return engagements list
        engs
        (let* ((man (car free-m))
               (pref-list (get-pref-list mpref man)))
          ; iterate through woman's list
          (let women-loop ((women pref-list))
            (let* ((woman (car women))
                   (couple (cons woman man)))
              (cond
                ; if list is empty (impossible case i hope), leave the man single
                ((null? women) (men-loop (cdr free-m) engs))
                ; if current woman is single, couple with single man
                ((null? (filter (lambda (x) (equal? woman (car x))) engs)) (men-loop (cdr free-m) (cons couple engs)))
                ; if the woman is married, there is a better partner, and the current man is one of them, couple them and leave previous husband single
                ((preferable? (get-pref-list wpref woman) man (get-partner engs woman))
                 (men-loop (cons (get-partner engs woman) (cdr free-m)) (update-engagements engs woman man)))
                ; else next woman
                (else (women-loop (cdr women))))))))))

; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))

; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (apply append (map (lambda (couple) (list (car couple) (cdr couple))) pair-list)))
 
