#lang racket

(include "recscm.scm")
(include "records.scm")
(include "tree.scm")

(define-record Nenv (l-id lval env))
(define-record Renv (l-id lexp env))
(define-record Closure (l-id body env))

(define make-cell (lambda (val) (mcons 'cell val)))
(define deref-cell mcdr)
(define set-cell! set-mcdr!)

(define member
    (lambda (x ids)
        (cond
            [(null? ids) #f]
            [(eq? x (car ids)) #t]
            [else (member x (cdr ids))])))

(define member-val
    (lambda (x ids vals)
        (cond
            [(null? ids) null]
            [(eq? x (car ids)) (car vals)]
            [else (member-val x (cdr ids) (cdr vals))])))


(define empty-env '())

(define get-ids (lambda (ll) (map cadr (map caddr ll))))
(define get-exps (lambda (ll) (map cadddr ll)))
(define get-cellexps (lambda (ll) (map (lambda (x) (make-cell (cadddr x))) ll)))
(define get-argids (lambda (ll) (map cadr ll)))

(define apply-env
    (lambda (tenv id)
        (record-case tenv
            (Nenv (l-id lval env)
                (if (member id l-id)
                    (force (deref-cell (member-val id l-id lval)))
                    (apply-env env id)))
            (Renv (l-id lexp env)
                (if (member id l-id)
                    (let*(  (ProcExpr (force (deref-cell (member-val id l-id lexp))))
                            (clos (eval-Expression ProcExpr env)))
                        (make-Closure (Closure->l-id clos) (Closure->body clos) tenv) )
                    (apply-env env id))))))

                    
                    
(define Nextend-env
    (lambda (env ids vals)
        (if (null? ids)
            env
            (make-Nenv ids vals env))))

(define Rextend-env
    (lambda (env ids exps)
        (if (null? ids)
            env
            (make-Renv ids exps env))))

(define update-env
    (lambda (tenv id val)
        (record-case tenv
            (Nenv (l-id lval env)
                (if (member id l-id)
                    (set-cell! (member-val id l-id lval) (eval-Expression (car val) (cdr val)))
                    (update-env env id val)))
            (Renv (l-id lexp env)
                (if (member id l-id)
                    (set-cell! (member-val id l-id lexp) (car val))
                    (update-env env id val))))))


(define eval-Expression
    (lambda (Expression env)
        (record-case Expression
            (IntegerLiteral (Token) (string->number Token))

            (TrueLiteral (Token) #t)

            (FalseLiteral (Token) #f)
            
            (Identifier (Token) (apply-env env Token))

            (PlusExpression (Token1 Token2 Expression1 Expression2 Token3)
                (+ (eval-Expression Expression1 env) (eval-Expression Expression2 env)))

            (IfExpression (Token1 Token2 Expression1 Expression2 Expression3 Token3)
                (if (eval-Expression Expression1 env) (eval-Expression Expression2 env) (eval-Expression Expression3 env)))

            (LetExpression (Token1 Token2 Token3 List Token4 Expression Token5)
                (let*(  (ids (get-ids List))
                        (exps (get-exps List))
                        (vals (map (lambda (exp) (make-cell (eval-Expression exp env))) exps))
                        (new-env (make-Nenv ids vals env)))
                    (eval-Expression Expression new-env)))
                        

            (Assignment (Token1 Token2 Identifier Expression Token3)
                (let*((val  (cons Expression env))
                      (id (cadr Identifier)))
                    (update-env env id val)))

            (ProcedureExp (Token1 Token2 Token3 List Token4 Expression Token5)
			    (make-Closure (get-argids List) Expression env))

            (Application (Token1 Expression List Token2)
                (let* ( (clos (eval-Expression Expression env))
                        (ids (Closure->l-id clos))
                        (vals (map (lambda (xp) (make-cell (delay (eval-Expression xp env)))) List))
                        (static-env (Closure->env clos))
                        (new-env (make-Nenv ids vals static-env))
                        (body (Closure->body clos)))
                    (eval-Expression body new-env)))
            
            (RecExpression (Token1 Token2 Token3 List Token4 Expression Token5)
                (let*(  (ids (get-ids List))
                        (exps (get-cellexps List))
                        (newrenv (make-Renv ids exps env)))
                    (eval-Expression Expression newrenv)))


            (else (error 'eval-Expression "Expression not found"))
            
            
)))

(define run
(lambda ()
	(record-case root
		(Goal (Expression Token)
		  (eval-Expression Expression empty-env))
		 (else (error 'run "Goal not found")))))
(run)

                    
            
               
        
        
        
        
        
