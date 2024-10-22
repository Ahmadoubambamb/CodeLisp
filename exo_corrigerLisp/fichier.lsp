(defun fact (n) (
   if(= n 0)
      1
       (* n (fact (- n 1)))
                )

)

;(fact 4)

(defun nb-bit (n)
   if(= n 0)
      0
      (+ 1 (nb-bit (ash (- n 1))))

)

(defun mymember (x ll)
  (
     if(atom ll)
        nil

        if(= x (car ll))
              ll
              (mymember x (car ll))
  )

  )



( defun supprimer (x l)
    ( if(atom l)
         l
         (if(= (car l) x)
          (supprimer x (cdr l))
          (cons (car l) (supprimer x (cdr l)))
         )
    )

)

(defun dernier(l)
  ( if(atom l)
       nil
       (if(null (cdr l))
           (car l)
           (dernier (cdr l))

       )

  )

)


 (defun compter (l)
    (if(atom l)
       nil
       (+ 1 (compter (cdr l)))
       )
 )

 (defun fact-terminal (n)
       (labels(fact-t (m r)
                  (if(= m 0)
                    r
                  (fact-t (- m 1) (* m r))
                    )
                      )
             (fact-t n 1))

 )


 (defun fact-term (m r)
                  (if(= m 0)
                    r
                  (fact-t (- m 1) (* m r))
                    )
                      )


(defun somme (&rest z)
     (if(atom z)
        0
        (+ (car z) (somme (cdr z))))

) ;debordement de pile donc on utilise apply pour eclater la liste


(defun somme (&rest z)
        ( if(atom z)
             0
             (+ (car z) (apply #'somme(cdr z)))
        )
)
; on peut aussi utilise label

(defun somme(&rest z)
   (labels((somme-rec (list-arg)
              ( if(atom list-arg)
                   0
                   (+ (car list-arg) (somme-rec (cdr list-arg)))
              )
           )
         )
   (somme-rec z)
                 )
)


(defun produit(&rest z)
   (labels((produit-rec (list-arg)
              ( if(atom list-arg)
                   0
                   (* (car list-arg) (produit-rec (cdr list-arg)))
              )
           )
         )
   (produit-rec z)
                 )
)



(defun produit-0 (&rest arg)
       (if(atom arg)
           1
           (* (car arg) (apply #'produit(cdr arg))))

)

( defun produit (list)
    ( if(atom list)
         1
         (* (car list) (produit (cdr list)))
)
)


(defun produit-1-* (n &rest z)
      (if(atom z)
          n
          (* n (apply #'produit-1-*((car z) (cdr z)))))

)


(defun prduit-01 (n &rest arg)
   (label ((produit-0 (arg)
       (if(atom arg)
           1
           (* (car arg) (produit-0 (cdr arg))))

)

   )

   )
  (* n (produit-0 arg))

)


(defun factoriel (n)
  (labels ( ( fact-01 (n r)
               ( if(= n 1)
                    r
                    (fact-01 (- n 1) (* n r))
               )
            )
          )
  (fact-01 n 1)
  )
)


;une fonction somme
(defun somme+ (&rest z)
   ( if(atom z)
        0
        (+ (car z) (apply #'somme+(cdr z))))
)

;En utilisant labels

(defun somme++ (&rest z)
  (labels ( (sommee+ (listarg)
               (if(atom listarg)
                    0
                    (+ (car listarg) (sommee+ (cdr listarg)))
               )
            )
          )

          (sommee+ z)
  )
)


(defun f (x)
   if()
)


(defun concat (l1 l2 &rest l3)
   (if(null l3)
      (append l1 l2)
      (append l1 (apply #'concat l2 l3))
   )
)

(defun concat (l1 l2 &rest l3)
   (if(atom l3)
      (append l1 l2)
      (append l1 (apply #'concat l2 (car l3) (cdr l3))
   )
)
)


(defun concat (l1 l2 &rest l3)
   (if(atom l3)
      (append l1 l2)
      (append (car l3) (apply #'concat l1 l2 (cdr l3))
       )
)
)

(defun concat (l1 l2 &rest l3)
   (if(atom l3)
        (append l1 l2)
      (apply #'concat l1 (append l2 (car l3)))
  )
)


(car (cdr (car (cdr (car '((a (b c)) e))))))
(cadadar '((a (b c)) e))

(defun iota (n)
      ( if(= n 0)
        '()
         (cons (- n 1) (iota (- n 1)))
      )
)

(defun dernier (l)
     (if(atom (cdr l))
        (car l)
        (dernier (cdr l))

     )
)

(defun avant-dernier (l)
    (if(atom (cddr l))
       (car l)
       (avant-dernier (cdr l))
    )
)

( defun union (ll ll1)
    (if(atom ll1)
        (append ll ll1)
        (if(atom (member (car ll1) ll))
             (cons (car ll1) (union ll (cdr ll1)))
          (append ll '())
        )
    )
)

(defun somme*2 (x y &optional z)
            (+ x y (or z 0))
)


 (defun somme*2 (x y &optional (z 0))

           (+ x y z)
           ; une fonction somme avec 2 ou 3 argument
)

( defun produit (x y &optional (z 1))
    (* x y z)
)

(defun inconnu (l)
   (cond
      ((null l) nil)
      ((null (cdr l)) nil)
      (T (cons (car l) (inconnu (cdr l))))
   )
)

(defun fact (n)
  (labels ((fact-1 (m r)
            (if(= m 0)
              r
             (fact-1 (- m 1) (* m r))
            )
         )
 )
  (fact-1 n 1)
))

(defun produit (L)
   (if(atom L)
       1
       (* (car L) (produit (cdr L)))
   )
)
;une version de recursion enveloper
(defun produit-term (l r)
     (if(atom l)
        r
        (produit-term (cdr l) (* r (car l))))
)
; on l'appel aevc comme argument une liste et l'entier

(loop for x in '(1 2 3)
      do (print x)
 )

 (loop for x in '(1 2 3)
  collect (* x 10)
 )

 (loop for i from 0 to 10
   do (print i)
 )

 (loop for x in '(1 2 3 4 5)
    while (< x 4)
      do (format t "x is ~a~&" x)
 )
