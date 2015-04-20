#lang scheme
#|
Author: Bacpfsec Rick
Date  : 20150420
|#

#|
Given input helper
|#
(define (read-keyboard-as-list)
            (let ((char (read-char)))
              (if (char=? char #\newline)
                  '()
                  (let loop ((char char))
                     (if (char=? char #\newline)
                         '()
                         (cons char (loop (read-char)))
                     )
                  )
              )
            )
  )


(define (read-keyboard-as-string)
            (let ((char (read-char)))
              (if (char=? char #\newline)
                  '()
                  (list->string
                   (let loop ((char char))
                     (if (char=? char #\newline)
                         '()
                         (cons char (loop (read-char)))
                     )
                   )
                  )
              )
            )
  )

#|
Global memories:
 Global stack
 Global variables
 Static functions
 Dynamic functions
 Helper for command lines
 Unique temporary
 Stack for static scoping
 String printing status
|#
(define global_stack '())
(set! global_stack (list))
(define running_vars '())
(set! running_vars '())
(define static_funcs '())
(set! static_funcs (list))
(define dynamic_funcs '())
(set! dynamic_funcs (list))
(define comd_list '())
(set! comd_list (list))
(define temp -1)
(set! temp -1)
(define backup '())
(set! backup (list))
(define ss 0)
(set! ss 0)

#|
General stack-based operations
|#
(define push (lambda (li ele) 
               (cons ele li)
               )
  )
(define drop (lambda (li)
               (cdr li)
               )
  )
(define pop (lambda (li)
              (set! temp (car li))
              (cdr li)
              )
  )
(define save (lambda (li)
               (cons temp li)
               )
  )
(define dup (lambda (li)
              (cons (car li) li)
              )
  )
(define swap (lambda (li)
               (cons (car (cdr li))
                     (cons (car li)
                           (cdr (cdr li))
                           )
                     )
               )
  )
(define rev (lambda (li)
              (reverse li)
              )
  )
(define clear (lambda (li)
                '())
  )             

#|
Basic math operations
|#
(define (add li)
  (if (>= (length li) 2)
  (let ( (x (car (cdr li))) )
    (set! x
          (+ x (car li)))
    (cons x (cdr (cdr li)))
    )
  (display "Need more arguments\n"))
  )
(define (minus li)
  (let ( (x (car (cdr li))) )
    (set! x
          (- x (car li)))
    (cons x (cdr (cdr li)))
    )
  )
(define (multiply li)
  (let ( (x (car (cdr li))) )
    (set! x
          (* x (car li)))
    (cons x (cdr (cdr li)))
    )
  )
(define (divide li)
  (let ( (x (car (cdr li))) )
    (set! x
          (/ x (car li)))
    (cons x (cdr (cdr li)))
    )
  )
(define (lessthan li)
  (let ( (x (car (cdr li))) )
    (set! x
          (< x (car li)))
    (cons x (cdr (cdr li)))
    )
  )
(define (greaterthan li)
  (let ( (x (car (cdr li))) )
    (set! x
          (> x (car li)))
    (cons x (cdr (cdr li)))
    )
  )
(define (lesseq li)
  (let ( (x (car (cdr li))) )
    (set! x
          (<= x (car li)))
    (cons x (cdr (cdr li)))
    )
  )
(define (greatereq li)
  (let ( (x (car (cdr li))) )
    (set! x
          (>= x (car li)))
    (cons x (cdr (cdr li)))
    )
  )

#|
Helpers for if-else and loop
|#
(define reach_else (lambda (li)                
                     (if (string? (car li))
                         (if (string=? "ELSE" (car li))
                             li
                             (reach_else (cdr li)))
                         (reach_else (cdr li)))
                     )
  )
(define reach_then (lambda (li)                
                     (if (string? (car li))
                         (if (string=? "THEN" (car li))
                             li
                             (reach_then (cdr li)))
                         (reach_then (cdr li)))
                     )
  )                
(define reach_pool (lambda (li)                
                     (if (string? (car li))
                         (if (string=? "POOL" (car li))
                             li
                             (reach_pool (cdr li)))
                         (reach_pool (cdr li)))
                     )
  )  
(define find_index (lambda (li key)                
                     (cond 
                       ((equal? li '())
                        -1)
                       ((equal? key (car li))
                         1)
                       (else
                         (+ (find_index (cdr li) key) 1))
                       )
                     )
  )
(define double_loop (lambda (li)
                      (append li 
                              (take li (find_index li "POOL"))
                              )
                      )
  )

#|
Helpers to stack management
|#
(define find_string_in_stack (lambda (li str)
               (if (= 0 (length li))
                   #f
                   (if (string=? str (car li))
                       #t
                       (find_string_in_stack (cdr li) str)
                       )
                   )
               )
  )
(define find_in_pair_stack (lambda (li key)
                            (if (= 0 (length li))
                                #f
                                (if (string=? key (car (car li)))
                                    (car (cdr (car li)))
                                    (find_in_pair_stack (cdr li) key)
                                    )
                                )
                            )
  )               
(define find_func_index (lambda (li key)                
                            (cond 
                              ((equal? li '())
                               -1)                               
                              ((equal? key (car (car li)))
                               1)
                              (else
                               (+ (find_func_index (cdr li) key) 1)
                               )
                              )
                            )
  )                            

#|
Helpers to define the variables
|#
(define (global_define li)
  (let ( (new_pair (list
                  (car (cdr comd_list))
                  (string->number (car (cdr (cdr comd_list))))
                  )
                 )
         )
    (set! comd_list (cdr comd_list))
    (set! comd_list (cdr comd_list))    
    (cons new_pair li)
    )
  )

#|
Helpers for function scoping
|#
(define static_func (lambda (l)
                        (cond ((equal? l '())
                               '())
                              ((equal? "(" (car l))
                               (static_func (cddddr (cdr l))))
                              (else
                               (append (list (car l)) (static_func (cdr l))))
                               )
                        )
    )
(define static_var (lambda (l)
                        (cond ((equal? l '())
                               '())
                              ((equal? "(" (car l))
                               (append (list (list (caddr l) 
                                                   (string->number (cadddr l))))
                                       (static_var (cddddr (cdr l)))))
                              (else
                               (static_var (cdr l)))
                               )
                        )
    )
(define static_build (lambda (l)
                       (list
                        (car l)
                        (static_func (cdr l))
                        (static_var (cdr l))
                        )
                       )
  )

#|
Helpers to process the command
|#
(define (read_comd) (let ( (input (read-keyboard-as-string)))
                      (set! comd_list
                            (cond
                              ((equal? input '())
                               '())
                              (else
                               (string-split input)))
                            )
                      )
  )

(define (print_comd)
  (print comd_list)
  )

(define (exec_comd) 
  (let ( (curr 0) (cache '()) (st 0))
    (cond
      ( (> (length comd_list) 0)
        (set! curr (car comd_list))
        ( cond            
           #| basic math operations |#
           ( (string=? "+" curr) 
             (set! global_stack (add global_stack)))
           ( (string=? "-" curr) 
             (set! global_stack (minus global_stack)))
           ( (string=? "*" curr) 
             (set! global_stack (multiply global_stack)))
           ( (string=? "/" curr) 
             (set! global_stack (divide global_stack)))
           ( (string=? "<" curr) 
             (set! global_stack (lessthan global_stack)))
           ( (string=? ">" curr) 
             (set! global_stack (greaterthan global_stack)))
           ( (string=? "<=" curr) 
             (set! global_stack (lesseq global_stack)))
           ( (string=? ">=" curr) 
             (set! global_stack (greatereq global_stack)))
           #| stack related operatinos |#
           ( (string=? "DROP" curr)
             (set! global_stack (drop global_stack)))
           ( (string=? "POP" curr)
             (set! global_stack (pop global_stack)))
           ( (string=? "SAVE" curr)
             (set! global_stack (save global_stack)))
           ( (string=? "DUP" curr)
             (set! global_stack (dup global_stack)))
           ( (string=? "SWAP" curr)
             (set! global_stack (swap global_stack)))
           ( (string=? "REV" curr)
             (set! global_stack (rev global_stack)))
           ( (string=? "STACK" curr)
             (print global_stack)
             (display "\n"))
           ( (string=? "CLEAR" curr)
             (set! global_stack (clear global_stack)))
           #| condition for show top |#
           ( (string=? "." curr)
             (cond 
               ((> (length comd_list) 1)
                (cond ((equal? #\"                               
                               (car (string->list (car (cdr comd_list)))))                       
                       (cond 
                         #|Single word|#
                         ((equal? #\"
                                  (last (string->list (car (cdr comd_list)))))
                          (display (car (cdr comd_list)))
                          (display "\n")
                          (set! comd_list (cdr comd_list))
                          )
                         #|Double Word|#                       
                         (else
                          (display (car (cdr comd_list)))
                          (display " ")
                          (set! comd_list (cdr comd_list))
                          (set! ss 1)                       
                          )
                         )
                       )                                                     
                      (else
                       (print (car global_stack))                       
                       (display "\n"))
                 ))
               (else
                (print (car global_stack) ) 
                (display "\n"))
               )
             )             
           #|Double Word|#
           ( (equal? ss 1)                     
             (cond (
                    (equal? #\"
                            (last (string->list (car comd_list))))
                    (display (car comd_list))                    
                    (display "\n")
                    (set! ss 0)
                    )
                   (else
                    (display (car comd_list))
                    (display " ")
                    )
                   )
             )
           #| push number into stack |#
           ( (string->number (car comd_list))
             (set! global_stack 
                   (push global_stack 
                         (string->number (car comd_list)))))
           #| push defined vars to stack |#
           ( (not (false? (find_in_pair_stack running_vars curr))) #| find a global var|#
             (set! global_stack
                   (push global_stack (find_in_pair_stack running_vars curr))))  
           #| if-else condition |#
           ( (string=? "IF" curr)             
             (cond
               ((not (car global_stack)) #| #f |#                 
                 (set! comd_list (reach_else comd_list))
                 )
               )
             (set! global_stack (pop global_stack)))
           ( (string=? "ELSE" curr)
             (set! comd_list (reach_then comd_list)))
           ( (string=? "THEN" curr))
           #| handle the loop issue |#
           ( (string=? "LOOP" curr)
             (cond 
               ((false? (car global_stack))
                (set! comd_list (reach_pool comd_list)))
               (else
                (set! comd_list (double_loop comd_list)))
                ))
           ( (string=? "POOL" curr))
           #| global variable define |#
           ( (string=? "define" curr)
             (cond 
               ((not (false? (find_in_pair_stack running_vars curr))) #| already define |#
                 (print "Already defined"))
               (else
                 (set! running_vars (global_define running_vars)))
               ))
           #| define static fucntion |#
           ( (string=? "FUNC$" curr)
             (set! static_funcs
                   (cons (static_build (cdr comd_list)) static_funcs))
             (set! comd_list (list "Func$" "CNUF")))
           ( (string=? "CNUF" curr))
           #| call static fucntion |#
           ( (> (find_func_index static_funcs curr) 0)
             (set! backup running_vars)
             (set! cache (last (take static_funcs
                                    (find_func_index static_funcs curr))))
             (cond
               ((not (equal? '() (cadr cache)))
                (set! comd_list (append (list "VOID") 
                                        (cadr cache) 
                                        (cdr comd_list)
                                        (list "RECO"))))
               )
             (cond
               ((not (equal? '() (caddr cache)))
                (set! running_vars (append (caddr cache) running_vars)))
               )
             )
           ( (string=? "RECO" curr)
             (set! running_vars backup)
             )
           #| define dynamic function |#
           ( (string=? "FUNC%" curr)
             (set! dynamic_funcs
                   (cons (static_build (cdr comd_list)) dynamic_funcs))
             (set! comd_list (list "Func%" "CNUF")))
           ( (string=? "CNUF" curr))
           #| call dynamic fucntion |#
           ( (> (find_func_index dynamic_funcs curr) 0)
             (set! cache (last (take dynamic_funcs
                                    (find_func_index dynamic_funcs curr))))
             (cond
               ((not (equal? '() (cadr cache)))
                (set! comd_list (append (list "VOID") 
                                        (cadr cache) 
                                        (cdr comd_list))))
               )
             (cond
               ((not (equal? '() (caddr cache)))
                (set! running_vars (append (caddr cache) running_vars)))
               )
             )
           #| invalid command |#
           ( else
              (display "Command Undefined\n"))
           )
        (set! comd_list (cdr comd_list)) #|proceed to next command|#
        (exec_comd)
        )
      )  
    )
  )

#|
Function: Lang ,runner of the program
|#
(define (Lang) 
  (display "Lang> ")
  (read_comd)
  (cond
    ((not (equal? comd_list '()))
     (exec_comd)
     (Lang))
    )
  )

(UofL)