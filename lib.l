# 06oct14abu
# (c) Software Lab. Alexander Burger

### Symbol ###
(de loc (S X)
   (if (and (str? X) (= S X))
      X
      (and
         (pair X)
         (or
            (loc S (car X))
            (loc S (cdr X)) ) ) ) )

### Check ###
# Unit tests
(de test (Pat . Prg)
   (bind (fish pat? Pat)
      (unless (match Pat (run Prg 1))
         (msg Prg)
         (quit 'fail Pat) ) ) )

### Debug ###
`*Dbg

(de getd ("X")
   (and
      (sym? "X")
      (fun? (val "X"))
      (val "X") ) )

(de expr ("F")
   (set "F"
      (list '@ (list 'pass (box (getd "F")))) ) )

(de subr ("F")
   (set "F"
      (getd (cadr (cadr (getd "F")))) ) )

(load "@lib/debug.l")

# vi:et:ts=3:sw=3
