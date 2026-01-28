#lang racket/base

(require racket/list
         racket/string
         scribble/manual
         scribble/struct
         scribble/basic
         scribble/scheme              ; value-link-color, racketidfont, symbol-color, etc.
         scribble/manual-struct       ; make-exported-index-desc*
         scribble/private/manual-vars    ; add-background-label, boxed-style, etc.
         scribble/private/manual-bind    ; id-to-target-maker, annote-exporting-library
         scribble/private/qsloc          ; quote-syntax/loc
         (for-syntax racket/base
                     syntax/parse
                     scribble/private/qsloc)  ; quote-syntax/loc for macro expansion
         (for-label racket/base
                    hash-view))

(provide defhashview)

(define spacer (hspace 1))
(define (to-flow e) (list (make-omitable-paragraph (list e))))
(define flow-spacer (to-flow spacer))

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

  ;; Width of optional field if combined on one line: [name #:keyword ....]
  (define (optional-field-combined-width f)
    (+ 1  ; [
       (sym-length (field-name f))
       1  ; space
       (if (eq? (field-default-mode f) 'default) 8 13)  ; #:default or #:default/omit
       1  ; space
       4  ; ....
       1)) ; ]

  ;; Threshold for combining optional field on one line
  (define combine-threshold 50)

  ;; Compute field view for single-line format: symbol or [symbol #:default ....] for optional
  (define (field-view f)
    (define fname (field-name f))
    (define mode (field-default-mode f))
    (cond
      [(not mode) fname]
      [(eq? mode 'default)
       (make-shaped-parens (list fname '#:default '....) #\[)]
      [(eq? mode 'default/omit)
       (make-shaped-parens (list fname '#:default/omit '....) #\[)]))

  ;; For multi-line format: opening part of field (name, with [ for optional)
  (define (field-open f)
    (define fname (field-name f))
    (define mode (field-default-mode f))
    (if mode
        (make-element #f (list (racketparenfont "[") (to-element fname)))
        (to-element fname)))

  ;; For multi-line format: render optional field as single combined element
  (define (field-combined f)
    (define fname (field-name f))
    (define mode (field-default-mode f))
    (make-element #f
      (list (racketparenfont "[")
            (to-element fname)
            spacer
            (to-element (if (eq? mode 'default) '#:default '#:default/omit))
            spacer
            (to-element '....)
            (racketparenfont "]"))))

  ;; For multi-line format: closing part of optional field (#:keyword ....])
  ;; Returns #f for required fields
  (define (field-keyword-line f)
    (define mode (field-default-mode f))
    (and mode
         (make-element #f (list (to-element (if (eq? mode 'default) '#:default '#:default/omit))
                                spacer
                                (to-element '....)
                                (racketparenfont "]")))))

  ;; Build the name element with cross-reference targets
  (define the-name
    (cond
      [link?
       (define target-maker (id-to-target-maker stx-id #t))
       (define content (annote-exporting-library (to-element #:defn? #t stx-id)))
       (define ref-content (to-element stx-id))
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
                   (case (field-default-mode f)
                     [(#f) 0]                    ; required field
                     [(default) (+ 2 1 8 1 4)]   ; [name #:default ....]
                     [(default/omit) (+ 2 1 13 1 4)])))))
       (if (eq? mutability 'immutable) 11 0)))  ; " #:immutable"

  ;; Should we use multi-line layout?
  ;; Reserve space for the "hash-view" label in the upper right
  (define short? (short-width . < . (- max-proto-width 15)))

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

       ;; Helper: convert field to item list (1 item for required, 1-2 for optional)
       (define (field->items f)
         (cond
           [(not (field-default-mode f))
            (list (list 'open f))]
           [((optional-field-combined-width f) . <= . combine-threshold)
            (list (list 'combined f))]
           [else
            (list (list 'open f) (list 'keyword f))]))

       ;; Determine which item gets the closing parens
       (define last-item-index
         (sub1 (for/sum ([f (in-list fields)])
                 (if (field-default-mode f) 2 1))))

       ;; Helper to render a field item
       (define (render-item item idx)
         (define type (car item))
         (define f (cadr item))
         (define last? (and (= idx last-item-index) (not immutable-follows?)))
         (define close (if last? closing-parens ""))
         (cond
           [(eq? type 'open)
            (make-element 'no-break
                          (list (field-open f) close))]
           [(eq? type 'combined)
            (make-element 'no-break
                          (list (field-combined f) close))]
           [(eq? type 'keyword)
            (make-element 'no-break
                          (list spacer (field-keyword-line f) close))]))

       ;; First field's items (at least 1, possibly 2 if optional)
       (define first-field (car fields))
       (define first-field-items (field->items first-field))
       (define remaining-items
         (if (null? (cdr fields))
             (cdr first-field-items)  ; just keyword row if first field is optional
             (append (cdr first-field-items)
                     (append-map field->items (cdr fields)))))

       (list
        (list
         ((add-background-label "hash-view")
          (list
           (make-table
            #f
            (append
             ;; First row: "(hash-view name" and possibly "(" and first field-open
             (list
              (append
               (list (to-flow (make-element #f (list (racketparenfont "(")
                                                     (racket hash-view))))
                     flow-spacer)
               (if split-field-line?
                   ;; Just the name on first line
                   (list (to-flow (make-element 'no-break the-name)))
                   ;; Name, "(", and first field-open on first line
                   (list (to-flow (make-element 'no-break the-name))
                         (to-flow (make-element #f (list spacer (racketparenfont "("))))
                         (to-flow (render-item (car first-field-items) 0))))))

             ;; First field on its own line (if split)
             (if split-field-line?
                 (list
                  (list flow-spacer flow-spacer
                        (to-flow (make-element
                                  #f
                                  (list (racketparenfont "(")
                                        (render-item (car first-field-items) 0))))))
                 null)

             ;; Remaining items (keyword lines for first field + all items for other fields)
             (for/list ([item (in-list remaining-items)]
                        [i (in-naturals 1)])
               (append
                (list flow-spacer flow-spacer)
                (if split-field-line? null (list flow-spacer flow-spacer))
                (list (to-flow (make-element
                                #f
                                (list (if split-field-line? spacer null)
                                      (render-item item i)))))))

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
                        (to-flow "=")
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
