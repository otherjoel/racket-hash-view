#lang racket/base

(require racket/string
         scribble/manual
         scribble/struct
         scribble/basic
         scribble/scheme              ; value-link-color, racketidfont, symbol-color, etc.
         scribble/manual-struct       ; make-exported-index-desc*
         scribble/private/manual-vars    ; add-background-label, boxed-style, etc.
         scribble/private/manual-bind    ; id-to-target-maker, annote-exporting-library
         scribble/private/manual-utils   ; flow-spacer, spacer, to-flow, etc.
         scribble/private/qsloc          ; quote-syntax/loc
         (for-syntax racket/base
                     syntax/parse
                     scribble/private/qsloc)  ; quote-syntax/loc for macro expansion
         (for-label racket/base
                    hash-view))

(provide defhashview)

;; ============================================================
;; Helper: make-target-element* (adapted from scribble/private/manual-proc.rkt)
;;
;; Creates nested target elements for cross-referencing.
;; `wrappers` is a list of (list kind-symbol name-part ...)
;; e.g., (list 'info name) or (list 'predicate name '?)

(define (make-target-element* inner-make-target-element stx-id content wrappers)
  (if (null? wrappers)
      content
      (make-target-element*
       make-target-element
       stx-id
       (let* ([name (datum-intern-literal (string-append* (map symbol->string (cdar wrappers))))]
              [target-maker
               (id-to-target-maker (datum->syntax stx-id (string->symbol name))
                                   #t)]
              [is-hash-view? (eq? 'info (caar wrappers))])
         (if target-maker
             (target-maker
              content
              (lambda (tag)
                (inner-make-target-element
                 #f
                 (make-index-element
                  #f
                  content
                  tag
                  (list name)
                  (list (let ([name (racketidfont (make-element value-link-color
                                                                (list name)))])
                          (if is-hash-view?
                              (list name " " (element 'smaller "(hash-view)"))
                              (list name))))
                  (with-exporting-libraries
                   (lambda (libs)
                     (let ([name (string->symbol name)])
                       (make-exported-index-desc*
                        name
                        libs
                        (hash 'kind (if is-hash-view?
                                        "hash-view"
                                        "procedure")))))))
                 tag)))
             content))
       (cdr wrappers))))

;; ============================================================
;; Main macro: defhashview

(begin-for-syntax
  ;; Syntax class for field specifications
  (define-syntax-class field-spec
    #:attributes (name contract default-mode default-expr)
    ;; Required field: [field contract]
    (pattern [name:id contract:expr]
             #:with default-mode #'#f
             #:with default-expr #'#f)
    ;; Optional with #:default: [field contract #:default expr]
    (pattern [name:id contract:expr #:default default-expr:expr]
             #:with default-mode #''default)
    ;; Optional with #:default/omit: [field contract #:default/omit expr]
    (pattern [name:id contract:expr #:default/omit default-expr:expr]
             #:with default-mode #''default/omit)))

(define-syntax (defhashview stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:link-target? link-target?-expr)
                   #:defaults ([link-target?-expr #'#t]))
        name:id (fld:field-spec ...)
        (~optional (~or* (~and #:immutable immutable-kw)
                         (~and #:accept-mutable accept-mutable-kw)))
        desc ...)
     (define mutability
       (cond [(attribute immutable-kw) 'immutable]
             [else 'accept-mutable]))
     (with-syntax ([mutability-val mutability])
       #'(with-togetherable-racket-variables
          ()
          ()
          (*defhashview link-target?-expr
                        (quote-syntax/loc name)
                        'name
                        (list (list 'fld.name fld.default-mode) ...)
                        (list (lambda () (racketblock0 fld.contract)) ...)
                        (list (and fld.default-expr (lambda () (racketblock0 fld.default-expr))) ...)
                        'mutability-val
                        (lambda () (list desc ...)))))]))

;; ============================================================
;; Runtime function: *defhashview

(define (*defhashview link? stx-id name fields field-contracts field-defaults mutability content-thunk)
  (define max-proto-width (current-display-width))

  (define (field-name f) (car f))
  (define (field-default-mode f) (cadr f))  ; #f, 'default, or 'default/omit
  (define (sym-length s) (string-length (symbol->string s)))

  ;; Compute field view (symbol or [symbol] for optional)
  (define (field-view f)
    (define fname (field-name f))
    (if (field-default-mode f)
        (make-shaped-parens (list fname) #\[)
        fname))

  ;; Create cross-reference targets
  (define target-wrappers
    (list* (list 'info name)
           (list 'predicate name '?)
           (list 'constructor 'make- name)
           (append
            (if (eq? mutability 'accept-mutable)
                (list (list 'constructor 'make-mutable- name))
                null)
            (for/list ([f (in-list fields)])
              (list 'accessor name '- (field-name f))))))

  ;; Build the name element with cross-reference targets
  (define the-name
    (cond
      [link?
       (define target-maker (id-to-target-maker stx-id #t))
       (define content (annote-exporting-library (to-element #:defn? #t stx-id)))
       (define ref-content (to-element stx-id))
       (if target-maker
           (make-target-element*
            (lambda (s c t) (make-toc-target2-element s c t ref-content))
            stx-id
            content
            target-wrappers)
           content)]
      [else
       (to-element #:defn? #t stx-id)]))

  ;; Compute width for single-line format
  (define short-width
    (+ 12  ; "(hash-view " + ")"
       (sym-length name)
       1   ; space before field list
       2   ; parens around field list
       (if (null? fields)
           0
           (+ (sub1 (length fields))  ; spaces between fields
              (for/sum ([f (in-list fields)])
                (+ (sym-length (field-name f))
                   (if (field-default-mode f) 2 0)))))  ; brackets for optional
       (if (eq? mutability 'immutable) 11 0)))  ; " #:immutable"

  ;; Should we use multi-line layout?
  (define short? (short-width . < . max-proto-width))

  ;; For multi-line: should fields start on a new line after the name?
  (define split-field-line?
    (and (not short?)
         (pair? fields)
         (max-proto-width . < . (+ 13  ; "(hash-view " + " ("
                                   (sym-length name)
                                   (sym-length (field-name (car fields)))
                                   1))))

  ;; Build the prototype row(s)
  (define prototype-rows
    (cond
      [short?
       ;; Single-line format
       (list
        (list
         ((add-background-label "hash-view")
          (list
           (make-omitable-paragraph
            (list
             (to-element
              (append
               (list (racket hash-view)
                     the-name
                     (to-element (map field-view fields)))
               (if (eq? mutability 'immutable)
                   (list (racket #:immutable))
                   null)))))))))]
      [else
       ;; Multi-line format: one cell containing an inner table with multiple rows
       (define immutable-follows? (eq? mutability 'immutable))
       (define closing-parens
         (racketparenfont (if immutable-follows? ")" "))")))
       (list
        (list
         ((add-background-label "hash-view")
          (list
           (make-table
            #f
            (append
             ;; First row: "(hash-view name" and possibly "(" and first field
             (list
              (append
               (list (to-flow (make-element #f (list (racketparenfont "(")
                                                     (racket hash-view))))
                     flow-spacer)
               (if split-field-line?
                   ;; Just the name on first line
                   (list (to-flow (make-element 'no-break the-name)))
                   ;; Name, "(", and first field on first line
                   (list (to-flow (make-element 'no-break the-name))
                         (to-flow (make-element #f (list spacer (racketparenfont "("))))
                         (to-flow (make-element
                                   'no-break
                                   (let ([f (to-element (field-view (car fields)))])
                                     (if (null? (cdr fields))
                                         (list f closing-parens)
                                         f))))))))
             ;; First field on its own line (if split)
             (if split-field-line?
                 (list
                  (list flow-spacer flow-spacer
                        (to-flow (make-element
                                  'no-break
                                  (list (racketparenfont "(")
                                        (let ([f (to-element (field-view (car fields)))])
                                          (if (null? (cdr fields))
                                              (list f closing-parens)
                                              f)))))))
                 null)
             ;; Remaining fields
             (let* ([remaining (cdr fields)]
                    [last-index (sub1 (length remaining))])
               (for/list ([f (in-list remaining)]
                          [i (in-naturals)])
                 (define last? (= i last-index))
                 (append
                  (list flow-spacer flow-spacer)
                  (if split-field-line? null (list flow-spacer flow-spacer))
                  (list (to-flow (make-element
                                  'no-break
                                  (list (if split-field-line? spacer null)
                                        (let ([fv (to-element (field-view f))])
                                          (if last?
                                              (list fv closing-parens)
                                              fv)))))))))
             ;; #:immutable keyword row (if needed)
             (if immutable-follows?
                 (list
                  (append
                   (list (to-flow (hspace 2)))
                   (if split-field-line?
                       (list flow-spacer)
                       (list flow-spacer flow-spacer flow-spacer))
                   (list (to-flow (make-element #f (list (to-element '#:immutable)
                                                         (racketparenfont ")")))))))
                 null)))))))]))

  ;; Build the main table
  (define main-table
    (make-table
     boxed-style
     (append
      prototype-rows

      ;; Field contract rows
      (for/list ([f (in-list fields)]
                 [fc (in-list field-contracts)]
                 [fd (in-list field-defaults)])
        (define fname (field-name f))
        (define default-mode (field-default-mode f))
        (list
         (make-flow
          (list
           (make-table
            #f
            (list
             (append
              (list (to-flow (hspace 2))
                    (to-flow (to-element (make-var-id fname)))
                    flow-spacer
                    (to-flow ":")
                    flow-spacer
                    (make-flow (list (fc))))
              (if fd
                  (list flow-spacer
                        (to-flow (case default-mode
                                   [(default) (racket #:default)]
                                   [(default/omit) (racket #:default/omit)]))
                        flow-spacer
                        (make-flow (list (fd))))
                  null)))))))))))

  ;; Assemble the final documentation block
  (make-box-splice
   (cons
    (make-blockquote
     vertical-inset-style
     (list main-table))
    (content-thunk))))
