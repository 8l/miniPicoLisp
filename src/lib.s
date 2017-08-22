# 15jul15abu
# (c) Software Lab. Alexander Burger

macro ("Prg"
   (run (fill "Prg")) )

recur (recurse
   (run (cdr recurse)) )

curry ("Z"
   (let ("X" (pop '"Z")  "Y" (pop '"Z")  "P" (filter pat? "X"))
      (if2 "P" (diff "X" "P")
         (list "Y" (cons 'job (lit (env @)) (fill "Z" "P")))
         (cons "Y" (fill "Z" "P"))
         (list "Y" (cons 'job (lit (env @)) "Z"))
         (cons "Y" "Z") ) ) )

### Definitions ###
undef (("X" "C")
   (when (pair "X")
      (setq  "C" (cdr "X")  "X" (car "X")) )
   (ifn "C"
      (prog1 (val "X") (set "X"))
      (prog1
         (cdr (asoq "X" (val "C")))
         (set "C"
            (delq (asoq "X" (val "C")) (val "C")) ) ) ) )

redef ("Lst"
   (let ("Old" (car "Lst")  "New" (name "Old"))
      (set
         "New" (val "Old")
         "Old" "New"
         "Old" (fill (cdr "Lst") "Old") )
      "New" ) )

daemon (("X" . Prg)
   (prog1
      (if (pair "X")
         (method (car "X") (cdr "X"))
         (or (pair (getd "X")) (expr "X")) )
      (con @ (append Prg (cdr @))) ) )

patch (("Lst" "Pat" . "Prg")
   (bind (fish pat? "Pat")
      (recur ("Lst")
         (loop
            (cond
               ((match "Pat" (car "Lst"))
                  (set "Lst" (run "Prg")) )
               ((pair (car "Lst"))
                  (recurse @) ) )
            (NIL (cdr "Lst"))
            (T (atom (cdr "Lst"))
               (when (match "Pat" (cdr "Lst"))
                  (con "Lst" (run "Prg")) ) )
            (setq "Lst" (cdr "Lst")) ) ) ) )

### I/O ###
tab ((Lst . @)
   (for N Lst
      (let V (next)
         (and (gt0 N) (space (- N (length V))))
         (prin V)
         (and (lt0 N) (args) (space (- 0 N (length V)))) ) )
   (prinl) )

msg ((X . @)
   (out NIL
      (print X)
      (pass prinl)
      (flush) )
   X )

script ((File . @)
   (load File) )

### List ###
insert ((N Lst X)
   (conc
      (cut (dec N) 'Lst)
      (cons X)
      Lst ) )

remove ((N Lst)
   (conc
      (cut (dec N) 'Lst)
      (cdr Lst) ) )

place ((N Lst X)
   (conc
      (cut (dec N) 'Lst)
      (cons X)
      (cdr Lst) ) )

uniq ((Lst)
   (let R NIL
      (filter
         '((X) (not (idx 'R X T)))
         Lst ) ) )

group ((Lst)
   (make
      (for X Lst
         (if (assoc (car X) (made))
            (conc @ (cons (cdr X)))
            (link (list (car X) (cdr X))) ) ) ) )

### OOP ###
class (Lst
   (let L (val (setq *Class (car Lst)))
      (def *Class
         (recur (L)
            (if (atom (car L))
               (cdr Lst)
               (cons (car L) (recurse (cdr L))) ) ) ) ) )

object (("Sym" "Val" . @)
   (putl "Sym")
   (def "Sym" "Val")
   (while (args)
      (put "Sym" (next) (next)) )
   "Sym" )

extend (X
   (setq *Class (car X)) )

# Class variables
var (X
   (if (pair (car X))
      (put (cdar X) (caar X) (cdr X))
      (put *Class (car X) (cdr X)) ) )

var: (X
   (apply meta X This) )

### Math ###
scl (("N" . "Prg")
   (if "Prg"
      (let *Scl "N" (run "Prg"))
      (setq *Scl "N") ) )

### Pretty Printing ###
pretty ((X N)
   (setq N (abs (space (or N 0))))
   (while (and (pair X) (== 'quote (car X)))
      (prin "'")
      (pop 'X) )
   (cond
      ((atom X) (print X))
      ((memq (car X) '(de dm))
         (_pretty
            (spPrt (pop 'X))
            (spPrt (pop 'X))
            (prtty1 X N Z) ) )
      ((memq (car X) '(let let?))
         (_pretty
            (cond
               ((atom (car X))
                  (spPrt (pop 'X))
                  (prtty? (pop 'X) N) )
               ((>= 12 (size (car X)))
                  (prin " (")
                  (let Z (pop 'X)
                     (prtty2 Z NIL Z) )
                  (prin ")") )
               (T
                  (nlPrt N)
                  (prin "(")
                  (let Z (pop 'X)
                     (prtty2 Z (+ N 3) Z) )
                  (prin " )") ) )
            (prtty1 X N Z) ) )
      ((== 'for (car X))
         (_pretty
            (cond
               ((or (atom (car X)) (atom (cdar X)))
                  (spPrt (pop 'X))
                  (prtty? (pop 'X) N) )
               ((>= 12 (size (car X)))
                  (spPrt (pop 'X)) )
               (T
                  (nlPrt N)
                  (prtty0 (pop 'X) (+ 3 N)) ) )
            (prtty1 X N Z) ) )
      ((== 'if2 (car X))
         (_pretty
            (when (>= 12 (size (head 2 X)))
               (spPrt (pop 'X))
               (spPrt (pop 'X)) )
            (prtty1 X N Z) ) )
      ((memq (car X) '(while until do state finally co))
         (prtty3 X N) )
      ((>= 12 (size X))
         (ifn (memq (car X) '(set setq default))
            (print X)
            (prin "(")
            (let Z X
               (printsp (pop 'X))
               (prtty2 X NIL Z) )
            (prin ")") ) )
      ((memq (car X) '(=: use later recur tab new))
         (_pretty
            (space)
            (print (pop 'X))
            (prtty1 X N Z) ) )
      ((memq (car X) '(set setq default))
         (_pretty
            (if (cdddr X)
               (prog
                  (nlPrt N)
                  (prtty2 X N Z) )
               (spPrt (pop 'X))
               (nlPrt1 (pop 'X) N) ) ) )
      ((memq (car X) '(T NIL ! if ifn when unless case casq with catch push bind job in out ctl))
         (prtty3 X N) )
      (T (prtty0 X N)) ) )

_pretty ("Prg"
   (prin "(")
   (let Z X
      (print (pop 'X))
      (run "Prg") )
   (prin " )") )

prtty0 ((X N)
   (prin "(")
   (let Z X
      (pretty (pop 'X) (- -3 N))
      (prtty1 X N Z) )
   (prin " )") )

prtty1 ((X N Z)
   (loop
      (NIL X)
      (T (== Z X) (prin " ."))
      (T (atom X) (prin " . ") (print X))
      (nlPrt1 (pop 'X) N) ) )

prtty2 ((X N Z)
   (loop
      (print (pop 'X))
      (NIL X)
      (T (== Z X) (prin " ."))
      (T (atom X) (prin " . ") (print X))
      (if N
         (prtty? (pop 'X) N)
         (space)
         (print (pop 'X)) )
      (NIL X)
      (T (== Z X) (prin " ."))
      (T (atom X) (prin " . ") (print X))
      (if N
         (nlPrt N)
         (space 2) ) ) )

prtty3 ((X N)
   (prin "(")
   (let Z X
      (print (pop 'X))
      (when (or (atom (car X)) (>= 12 (size (car X))))
         (spPrt (pop 'X)) )
      (when X
         (prtty1 X N Z)
         (space) ) )
   (prin ")") )

prtty? ((X N)
   (ifn (or (atom X) (>= 12 (size X)))
      (nlPrt1 X N)
      (spPrt X) ) )

spPrt ((X)
   (space)
   (print X) )

nlPrt ((N)
   (prinl)
   (space (+ 3 N)) )

nlPrt1 ((X N)
   (prinl)
   (pretty X (+ 3 N)) )

pp (("X" C)
   (let *Dbg NIL
      (pretty
         (if (or C (pair "X"))
            (cons 'dm "X"
               (if (pair "X")
                  (method (car "X") (cdr "X"))
                  (method "X" C) ) )
            (cons 'de "X" (val "X")) ) )
      (prinl)
      "X" ) )

show (("X" . @)
   (let *Dbg NIL
      (setq "X" (pass get "X"))
      (when (sym? "X")
         (print "X" (val "X"))
         (prinl)
         (maps
            '((X)
               (space 3)
               (if (atom X)
                  (println X)
                  (println (cdr X) (car X)) ) )
            "X" ) )
      "X" ) )
,((((@X) (^ @ (show (-> @X))))) . T)

view ((X L)
   (let (Z X  *Dbg)
      (loop
         (T (atom X) (println X))
         (if (atom (car X))
            (println '+-- (pop 'X))
            (print '+---)
            (view
               (pop 'X)
               (append L (cons (if X "|   " "    "))) ) )
         (NIL X)
         (mapc prin L)
         (T (== Z X) (println '*))
         (println '|)
         (mapc prin L) ) ) )

# vi:et:ts=3:sw=3
