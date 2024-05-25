#lang racket

; Grupo 5
; Murillo Pires Teixeira Melo (202300074) e Joao Alberto Bezerra de Albuquerque (202300071)

; funcoes auxiliares ---------------------------------------------------------------------------------------

(define (get-func-table code)
    (define (get-func code)
        (if (equal? (car code) 'end)
            '()
            (cons (car code) (get-func (cdr code)))
        )
    )

    (if (null? code)
        '()
        (if (equal? (car code) 'function)
            (cons (get-func (cdr code)) (get-func-table (cdr code)))
            (get-func-table (cdr code))
        )
    )
)

(define (lookup var vars-table) 
    (define (helper-lookup var vars-table-scope)
        (cond
            [(null? vars-table-scope) #f]
            [(equal? (caar vars-table-scope) var) (cadar vars-table-scope)]
            [else (helper-lookup var (cdr vars-table-scope))]
        )
    )
    (if (symbol? var)
        (if (null? vars-table)
            #f
            (if (false? (helper-lookup var (car vars-table)))
                (lookup var (cdr vars-table))
                (helper-lookup var (car vars-table))
            )
        )
        var
    )
)

(define (set-var-value var value vars-table) 
    (define (update-vars-list var value vars-list)
        (map 
            (lambda (pair)
                (if (equal? (car pair) var)
                    (list var value)
                    pair
                )
            )
            vars-list
        )
    )
    (define (copy-rest vars-table)
        (if (null? vars-table)
            '()
            (cons (car vars-table) (copy-rest (cdr vars-table)))
        )
    )
    (define (set-vars-helper var value vars-table)
        (let [(new-vars-list (update-vars-list var value (car vars-table)))]
            (cond
                [(lookup var (list new-vars-list)) (cons new-vars-list (copy-rest (cdr vars-table)))]
                [else (cons (car vars-table) (set-vars-helper var value (cdr vars-table)))]
            )
        )
    )
    (set-vars-helper var value vars-table)
)

(define (attr subcode-attr func-table vars-table) 
    (define (attr-simple subcode-attr vars-table)
        (let [(var (car subcode-attr)) (value (caddr subcode-attr))]
            (set-var-value var (lookup value vars-table) vars-table)
        )
    )
    (define (attr-expr subcode-attr vars-table)
        (let [(var (car subcode-attr)) (value1 (lookup (caddr subcode-attr) vars-table)) (op (cadddr subcode-attr)) (value2 (lookup (cadddr (cdr subcode-attr)) vars-table))]
            (cond
                [(equal? op '+) (set-var-value var (+ value1 value2) vars-table)]
                [(equal? op '-) (set-var-value var (- value1 value2) vars-table)]
                [(equal? op '*) (set-var-value var (* value1 value2) vars-table)]
                [(equal? op '/) (set-var-value var (/ value1 value2) vars-table)]
            )
        )
    )
    (define (attr-func subcode-attr vars-table func-table)
        (define (get-attr-vars subcode-attr)
            (if (equal? (car subcode-attr) '=)
                '()
                (cons (car subcode-attr) (get-attr-vars (cdr subcode-attr)))
            )
        )
        (define (go-to-func subcode-attr)
            (if (equal? (car subcode-attr) '=)
                (cdr subcode-attr)
                (go-to-func (cdr subcode-attr))
            )
        )
        (define (execute-helper attr-vars func-return vars-table)
            (cond
                [(and (null? func-return) (null? (cdr attr-vars))) (set-var-value (car attr-vars) 0 vars-table)]
                [(null? func-return) (execute-helper (cdr attr-vars) func-return (set-var-value (car attr-vars) 0 vars-table))]
                [(null? (cdr attr-vars)) (set-var-value (car attr-vars) (car func-return) vars-table)]
                [else (execute-helper (cdr attr-vars) (cdr func-return) (set-var-value (car attr-vars) (car func-return) vars-table))]
            )
        )
        (define (execute-attr attr-vars subcode-func-call func-table vars-table)
            (let [(func-return (func-call-attr subcode-func-call func-table vars-table))]
                (list (cadr func-return) (execute-helper attr-vars (car func-return) vars-table))
            )
        )
        (execute-attr (get-attr-vars subcode-attr) (go-to-func subcode-attr) func-table vars-table)
    )  
    (cond 
        [(or (equal? (cadddr subcode-attr) '!) (equal? (cadddr subcode-attr) 'fi)) (list func-table (attr-simple subcode-attr vars-table))]
        [(and (equal? (cadr subcode-attr) '=) (not (equal? (cadddr subcode-attr) '<))) (list func-table (attr-expr subcode-attr vars-table))]
        [else (attr-func subcode-attr vars-table func-table)]
    )
)

(define (return subcode-return func-table vars-table) 
    (define (return-helper subcode-return vars-table)
        (if (or (equal? (car subcode-return) '!) (equal? (car subcode-return) 'fi))
            '()
            (cons (lookup (car subcode-return) vars-table) (return-helper (cdr subcode-return) vars-table))
        )
    )
    (list (return-helper (cdr subcode-return) vars-table) func-table vars-table)
)

(define (conditional subcode-conditional func-table vars-table) 
    (define (if-test subcode-conditional-test vars-table)
        (let [(value1 (lookup (car subcode-conditional-test) vars-table)) (op (cadr subcode-conditional-test)) (value2 (lookup (caddr subcode-conditional-test) vars-table))]
            (cond 
                [(equal? op 'lt) (< value1 value2)]
                [(equal? op 'le) (<= value1 value2)]
                [(equal? op 'gt) (> value1 value2)]
                [(equal? op 'ge) (>= value1 value2)]
                [(equal? op 'eq) (= value1 value2)]
                [(equal? op 'ne) (not (= value1 value2))]
            )
        )
    )
    (define (if-command subcode-conditional-command func-table vars-table)
        (cond
            [(equal? (car subcode-conditional-command) 'return) (return subcode-conditional-command func-table vars-table)]
            [(equal? (cadr subcode-conditional-command) '<) (func-call subcode-conditional-command func-table vars-table)]
            [else (attr subcode-conditional-command func-table vars-table)]
        )
    )
    (if (if-test (cdr subcode-conditional) vars-table)
        (if-command (cdr (cddddr subcode-conditional)) func-table vars-table)
        (list func-table vars-table)
    )
)

(define (code-delete subcode-delete func-table vars-table) 
    (define (get-subcode-func name func-table)
        (if (equal? (caar func-table) name)
            (car func-table)
            (get-subcode-func name (cdr func-table))
        )
    )
    (define (go-to-begin subcode-func)
        (if (equal? (car subcode-func) 'begin)
            '(begin)
            (cons (car subcode-func) (go-to-begin (cdr subcode-func)))
        )
    )
    (define (get-commands-body subcode-func)
        (if (equal? (car subcode-func) 'begin)
            (cdr subcode-func)
            (get-commands-body (cdr subcode-func))
        )
    )
    (define (delete-line-commands-body line-number commands-body)
        (define (copy-command commands-body)
            (if (or (equal? (car commands-body) '!) (equal? (car commands-body) 'fi))
                (cons (car commands-body) '())
                (cons (car commands-body) (copy-command (cdr commands-body)))
            )
        )
        (define (skip-command commands-body)
            (if (or (equal? (car commands-body) '!) (equal? (car commands-body) 'fi))
                (cdr commands-body)
                (skip-command (cdr commands-body))
            )
        )
        (if (= line-number 1)
            (skip-command commands-body)
            (append (copy-command commands-body) (delete-line-commands-body (- line-number 1) (skip-command commands-body)))
        )
    )
    (define (modified-func-table modified-func func-table)
        (cond 
            [(null? func-table) '()]
            [(equal? (car modified-func) (caar func-table)) (cons modified-func (modified-func-table modified-func (cdr func-table)))]
            [else (car func-table) (cons (car func-table) (modified-func-table modified-func (cdr func-table)))]
        )
    )
    (let* [
        (func-name (cadr subcode-delete)) 
        (line-number (cadddr subcode-delete))
        (subcode-func (get-subcode-func func-name func-table))
        (func-head (go-to-begin subcode-func))
        (commands-body (get-commands-body subcode-func))
        (modified-commands-body (delete-line-commands-body line-number commands-body))
        (modified-func (append func-head modified-commands-body))
        ]
        (list (modified-func-table modified-func func-table) vars-table) 
    )
)

(define (code-update subcode-update func-table vars-table) 
    (define (get-subcode-func name func-table)
        (if (equal? (caar func-table) name)
            (car func-table)
            (get-subcode-func name (cdr func-table))
        )
    )
    (define (get-new-command subcode-update)
        (define (helper subcode-new-command)
            (if (or (equal? (car subcode-new-command) '!) (equal? (car subcode-new-command) 'fi))
                (list (car subcode-new-command))
                (cons (car subcode-new-command) (helper (cdr subcode-new-command)))
            )
        )
        (let [(subcode-new-command (cdr (cddddr subcode-update)))]
            (helper subcode-new-command)
        )
    )
    (define (go-to-begin subcode-func)
        (if (equal? (car subcode-func) 'begin)
            '(begin)
            (cons (car subcode-func) (go-to-begin (cdr subcode-func)))
        )
    )
    (define (get-commands-body subcode-func)
        (if (equal? (car subcode-func) 'begin)
            (cdr subcode-func)
            (get-commands-body (cdr subcode-func))
        )
    )
    (define (modify-line-commands-body line-number new-command commands-body)
        (define (copy-command commands-body)
            (if (or (equal? (car commands-body) '!) (equal? (car commands-body) 'fi))
                (cons (car commands-body) '())
                (cons (car commands-body) (copy-command (cdr commands-body)))
            )
        )
        (define (skip-command commands-body)
            (if (or (equal? (car commands-body) '!) (equal? (car commands-body) 'fi))
                (cdr commands-body)
                (skip-command (cdr commands-body))
            )
        )
        (if (= line-number 1)
            (append new-command (skip-command commands-body))
            (append (copy-command commands-body) (modify-line-commands-body (- line-number 1) new-command (skip-command commands-body)))
        )
    )
    (define (modified-func-table modified-func func-table)
        (cond 
            [(null? func-table) '()]
            [(equal? (car modified-func) (caar func-table)) (cons modified-func (modified-func-table modified-func (cdr func-table)))]
            [else (car func-table) (cons (car func-table) (modified-func-table modified-func (cdr func-table)))]
        )
    )
    (let* [
        (func-name (cadr subcode-update)) 
        (line-number (cadddr subcode-update))
        (subcode-func (get-subcode-func func-name func-table))
        (func-head (go-to-begin subcode-func))
        (commands-body (get-commands-body subcode-func))
        (new-command (get-new-command subcode-update))
        (modified-commands-body (modify-line-commands-body line-number new-command commands-body))
        (modified-func (append func-head modified-commands-body))
        ]
        (list (modified-func-table modified-func func-table) vars-table)
    )
)

(define (func-call-attr subcode-call func-table vars-table)
    (define (get-func-subcode subcode-call func-table)
        (if (equal? (car subcode-call) (caar func-table))
            (car func-table)
            (get-func-subcode subcode-call (cdr func-table))
        )
    )
    (define (set-new-vars-table subcode-call func-table vars-table)
        (define (get-vars func-subcode)
            (define (go-to-vars func-subcode)
                (if (equal? (car func-subcode) '>)
                    (cdr func-subcode)
                    (go-to-vars (cdr func-subcode))
                )
            )
            (define (get-vars-helper func-subcode-vars)
                (if (equal? (car func-subcode-vars) 'begin)
                    '()
                    (cons (list (car func-subcode-vars)) (get-vars-helper (cdr func-subcode-vars)))
                )
            )
            (let [(vars (get-vars-helper (go-to-vars func-subcode)))]
                (if (null? vars)
                    vars
                    (cdr vars)
                )
            ) 
        )
        (define (set-parameters subcode-call func-table)
            (define (get-parameters-values sub-subcode-call)
                (if (equal? (car sub-subcode-call) '>)
                    '()
                    (cons (lookup (car sub-subcode-call) vars-table) (get-parameters-values (cdr sub-subcode-call)))
                )
            )
            (define (get-parameters-names func-subcode-parameters)
                (if (equal? (car func-subcode-parameters) '>)
                    '()
                    (cons (car func-subcode-parameters) (get-parameters-names (cdr func-subcode-parameters)))
                )
            )
            (define (set-parameters-helper parameters-names parameters-values)
                (map (lambda (elem1 elem2) (list elem1 elem2))
                    parameters-names
                    parameters-values
                )
            )
            (let [(func-subcode-parameters (cddr (get-func-subcode subcode-call func-table)))]
                (set-parameters-helper (get-parameters-names func-subcode-parameters) (get-parameters-values (cddr subcode-call)))
            )
        ) 
        (let [(locals (append (set-parameters subcode-call func-table) (get-vars (get-func-subcode subcode-call func-table))))]
            (cons locals vars-table)
        )
    )
    (define (bnl-print subcode-print)
        (display (lookup (caddr subcode-print) vars-table))
        (newline)
        (list func-table vars-table)
    )
    (define (bnl-read func-table vars-table)
        (let [(input 0)]
            (set! input (read))
            (list (list input) func-table vars-table)
        )
    )
    (define (execute-body func-subcode func-table vars-table)
        (define (get-commands-body subcode-func)
            (if (equal? (car subcode-func) 'begin)
                (cdr subcode-func)
                (get-commands-body (cdr subcode-func))
            )
        )
        (define (execute-command commands-body func-table vars-table)
            (cond
                [(equal? (car commands-body) 'if) (conditional commands-body func-table vars-table)]
                [(equal? (car commands-body) 'return) (return commands-body func-table vars-table)]
                [(equal? (car commands-body) 'delete) (code-delete commands-body func-table vars-table)]
                [(equal? (car commands-body) 'update) (code-update commands-body func-table vars-table)]
                [(equal? (cadr commands-body) '<) (func-call commands-body func-table vars-table)]
                [else (attr commands-body func-table vars-table)]
            )
        )
        (define (skip-command commands-body)
            (if (or (equal? (car commands-body) '!) (equal? (car commands-body) 'fi))
                (cdr commands-body)
                (skip-command (cdr commands-body))
            )
        )
        (define (execute-body-helper commands-body func-table vars-table)
            (let [(command-result (execute-command commands-body func-table vars-table))]
                (if (null? (cddr command-result))
                    (execute-body-helper (skip-command commands-body) (car command-result) (cadr command-result))
                    command-result
                )
            )
        )
        (let* [(commands-body (get-commands-body func-subcode)) ]
            (execute-body-helper commands-body func-table vars-table)
        )
    )
    
    (cond 
        [(equal? (car subcode-call) 'print) (bnl-print subcode-call)]
        [(equal? (car subcode-call) 'read) (bnl-read func-table vars-table)]
        [else 
            (let [(func-subcode (get-func-subcode subcode-call func-table))]
                (let [(current-vars-table (set-new-vars-table subcode-call func-table vars-table))]
                    (let [(return (execute-body func-subcode func-table current-vars-table))]
                        (list (car return) (cadr return) (cdaddr return))
                    )
                )
            )
        ]
    )
)

(define (func-call subcode-call func-table vars-table)
    (define (get-func-subcode subcode-call func-table)
        (if (equal? (car subcode-call) (caar func-table))
            (car func-table)
            (get-func-subcode subcode-call (cdr func-table))
        )
    )
    (define (set-new-vars-table subcode-call func-table vars-table)
        (define (get-vars func-subcode)
            (define (go-to-vars func-subcode)
                (if (equal? (car func-subcode) '>)
                    (cdr func-subcode)
                    (go-to-vars (cdr func-subcode))
                )
            )
            (define (get-vars-helper func-subcode-vars)
                (if (equal? (car func-subcode-vars) 'begin)
                    '()
                    (cons (list (car func-subcode-vars)) (get-vars-helper (cdr func-subcode-vars)))
                )
            )
            (let [(vars (get-vars-helper (go-to-vars func-subcode)))]
                (if (null? vars)
                    vars
                    (cdr vars)
                )
            ) 
        )
        (define (set-parameters subcode-call func-table)
            (define (get-parameters-values sub-subcode-call)
                (if (equal? (car sub-subcode-call) '>)
                    '()
                    (cons (lookup (car sub-subcode-call) vars-table) (get-parameters-values (cdr sub-subcode-call)))
                )
            )
            (define (get-parameters-names func-subcode-parameters)
                (if (equal? (car func-subcode-parameters) '>)
                    '()
                    (cons (car func-subcode-parameters) (get-parameters-names (cdr func-subcode-parameters)))
                )
            )
            (define (set-parameters-helper parameters-names parameters-values)
                (map (lambda (elem1 elem2) (list elem1 elem2))
                    parameters-names
                    parameters-values
                )
            )
            (let [(func-subcode-parameters (cddr (get-func-subcode subcode-call func-table)))]
                (set-parameters-helper (get-parameters-names func-subcode-parameters) (get-parameters-values (cddr subcode-call)))
            )
        ) 
        (let [(locals (append (set-parameters subcode-call func-table) (get-vars (get-func-subcode subcode-call func-table))))]
            (cons locals vars-table)
        )
    )
    (define (bnl-print subcode-print)
        (display (lookup (caddr subcode-print) vars-table))
        (newline)
        (list func-table vars-table)
    )
    (define (bnl-read func-table vars-table)
        (let [(input 0)]
            (set! input (read))
            (list (list input) func-table vars-table)
        )
    )
    (define (execute-body func-subcode func-table vars-table)
        (define (get-commands-body subcode-func)
            (if (equal? (car subcode-func) 'begin)
                (cdr subcode-func)
                (get-commands-body (cdr subcode-func))
            )
        )
        (define (execute-command commands-body func-table vars-table)
            (cond
                [(equal? (car commands-body) 'if) (conditional commands-body func-table vars-table)]
                [(equal? (car commands-body) 'return) (return commands-body func-table vars-table)]
                [(equal? (car commands-body) 'delete) (code-delete commands-body func-table vars-table)]
                [(equal? (car commands-body) 'update) (code-update commands-body func-table vars-table)]
                [(equal? (cadr commands-body) '<) (func-call commands-body func-table vars-table)]
                [else (attr commands-body func-table vars-table)]
            )
        )
        (define (skip-command commands-body)
            (if (or (equal? (car commands-body) '!) (equal? (car commands-body) 'fi))
                (cdr commands-body)
                (skip-command (cdr commands-body))
            )
        )
        (define (execute-body-helper commands-body func-table vars-table)
            (let [(command-result (execute-command commands-body func-table vars-table))]
                (if (null? (cddr command-result))
                    (execute-body-helper (skip-command commands-body) (car command-result) (cadr command-result))
                    command-result
                )
            )
        )
        (let* [(commands-body (get-commands-body func-subcode)) ]
            (execute-body-helper commands-body func-table vars-table)
        )
    )
    
    (cond 
        [(equal? (car subcode-call) 'print) (bnl-print subcode-call)]
        [(equal? (car subcode-call) 'read) (bnl-read func-table vars-table)]
        [else 
            (let [(func-subcode (get-func-subcode subcode-call func-table))]
                (let [(current-vars-table (set-new-vars-table subcode-call func-table vars-table))]
                    (let [(return (execute-body func-subcode func-table current-vars-table))]
                        (if (null? (caddr return))
                            (list (cadr return) (caddr return))
                            (list (cadr return) (cdaddr return))
                        )
                    )
                )
            )
        ]
    )
)
; ----------------------------------------------------------------------------------------------------------


(define (execute code)
    (let [(func-table (get-func-table code)) (vars-table '())]
        (func-call '(main < > !) func-table vars-table)
        (void)
    )
)

(define code '(

    function f < >
    begin
        return 10 5 !
    end
    
    function main < >
    vars n1 n2
    begin
        n1 n2 = f < > !
        print < n1 > !
        print < n2 > ! 
        return 0 !
    end

    )
)

(execute code)
