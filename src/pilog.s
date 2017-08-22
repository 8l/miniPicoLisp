# 15jul15abu
# (c) Software Lab. Alexander Burger

# *Rule

be (CL
   (clause CL) )

clause ((CL)
   (with (car CL)
      (if (== *Rule This)
         (queue (:: T) (cdr CL))
         (=: T (cons (cdr CL)))
         (setq *Rule This) )
      This ) )

repeat (()
   (conc (get *Rule T) (get *Rule T)) )
,((NIL .) . T)

asserta ((CL)
   (push (prop CL 1 T) (cdr CL)) )

assertz ((CL)
   (queue (prop CL 1 T) (cdr CL)) )

retract ((X)
   (if (sym? X)
      (put X T)
      (put (car X) T
         (delete (cdr X) (get (car X) T)) ) ) )

rules (@
   (while (args)
      (let S (next)
         (for ((N . L) (get S T) L)
            (prin N " (be ")
            (print S)
            (for X (pop 'L)
               (space)
               (print X) )
            (prinl ")")
            (T (== L (get S T))
               (println '(repeat)) ) )
         S ) ) )

### Pilog Interpreter ###
goal (("CL" . @)
   (let "Env" '(T)
      (while (args)
         (push '"Env"
            (cons (cons 0 (next)) 1 (next)) ) )
      (while (and "CL" (pat? (car "CL")))
         (push '"Env"
            (cons
               (cons 0 (pop '"CL"))
               (cons 1 (eval (pop '"CL"))) ) ) )
      (cons
         (cons
            (conc (list 1 (0) NIL "CL" NIL) "Env") ) ) ) )

fail (()
   (goal '((NIL))) )

pilog (("CL" . "Prg")
   (for ("Q" (goal "CL") (prove "Q"))
      (bind @ (run "Prg")) ) )

solve (("CL" . "Prg")
   (make
      (if "Prg"
         (for ("Q" (goal "CL") (prove "Q"))
            (link (bind @ (run "Prg"))) )
         (for ("Q" (goal "CL") (prove "Q"))
            (link @) ) ) ) )

query (("Q" "Dbg")
   (use "R"
      (loop
         (NIL (prove "Q" "Dbg"))
         (T (=T (setq "R" @)) T)
         (for X "R"
            (space)
            (print (car X))
            (print '=)
            (print (cdr X))
            (flush) )
         (T (line)) ) ) )

? ("CL"
   (let "L"
      (make
         (while (nor (pat? (car "CL")) (lst? (car "CL")))
            (link (pop '"CL")) ) )
      (query (goal "CL") "L") ) )

### Basic Rules ###
true NIL
,((NIL) . T)

call NIL
,(((@P (2 (cons (-> @P))))) . T)

_or NIL
,((((@C) (3 (pop (-> @C)))) ((@C) (^ @ (not (val (-> @C)))) T (fail)) .) . T)

equal NIL
,((((@X @X))) . T)

different NIL
,((((@X @X) T (fail)) ((@ @))) . T)

permute NIL
,(((((@X) (@X))) ((@L (@X . @Y)) (delete @X @L @D) (permute @D @Y))) . T)

uniq NIL
,((((@B @X) (^ @ (not (idx (-> @B) (-> @X) T))))) . T)

asserta NIL
,((((@C) (^ @ (asserta (-> @C))))) . T)

assertz NIL
,((((@C) (^ @ (assertz (-> @C))))) . T)

retract NIL
,((((@C) (2 (cons (-> @C))) (^ @ (retract (list (car (-> @C)) (cdr (-> @C))))))) . T)

clause NIL
,(((("@H" "@B") (^ "@A" (get (-> "@H") T)) (member "@B" "@A"))) . T)

_for NIL
,((((@N @I @End @Step) (^ @ (if (>= (-> @End) (val (-> @I))) (> (inc (-> @I) (-> @Step)) (-> @End)) (> (-> @End) (dec (-> @I) (-> @Step))))) T (fail)) ((@N @I @End @Step) (^ @N (val (-> @I)))) .) . T)

lst NIL
,((((@V . @L) (^ @Lst (box (apply get (-> @L)))) (_lst @V @Lst))) . T)

_lst NIL
,((((@Val @Lst) (^ @ (not (val (-> @Lst)))) T (fail)) ((@Val @Lst) (^ @Val (pop (-> @Lst)))) .) . T)

_map NIL
,((((@Val @Lst) (^ @ (not (val (-> @Lst)))) T (fail)) ((@Val @Lst) (^ @Val (prog1 (val (-> @Lst)) (pop (-> @Lst))))) .) . T)

# vi:et:ts=3:sw=3
