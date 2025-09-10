;; License Template System - Reusable Licensing Templates
;; Enables brand owners to create and manage standardized license templates

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_TEMPLATE_NOT_FOUND (err u201))
(define-constant ERR_TEMPLATE_INACTIVE (err u202))
(define-constant ERR_INVALID_PARAMS (err u203))
(define-constant ERR_NOT_TEMPLATE_OWNER (err u204))
(define-constant ERR_TEMPLATE_NAME_EXISTS (err u205))
(define-constant ERR_BRAND_NOT_FOUND (err u206))

;; Data variables
(define-data-var template-counter uint u0)
(define-data-var brand-rights-contract principal tx-sender)

;; Duration types for templates
(define-constant DURATION_FIXED u1)
(define-constant DURATION_RENEWABLE u2)
(define-constant DURATION_PERPETUAL u3)

;; Template data structure
(define-map license-templates
  uint
  {
    owner: principal,
    brand-id: uint,
    name: (string-ascii 64),
    description: (string-ascii 256),
    terms-uri: (string-utf8 256),
    duration-type: uint,
    base-duration: uint,
    base-price: uint,
    royalty-percentage: uint,
    usage-rights: (string-ascii 256),
    territory: (string-ascii 64),
    is-active: bool,
    created-at: uint,
    updated-at: uint
  }
)

;; Template usage statistics
(define-map template-stats
  uint
  {
    licenses-issued: uint,
    total-revenue: uint,
    last-used: uint,
    avg-duration: uint
  }
)

;; Template name registry to prevent duplicates per owner
(define-map template-names
  { owner: principal, name: (string-ascii 64) }
  { template-id: uint }
)

;; License-to-template mapping for tracking
(define-map license-template-mapping
  uint ;; license-id
  {
    template-id: uint,
    issued-at: uint,
    actual-price: uint
  }
)

;; Read-only functions

(define-read-only (get-template (template-id uint))
  (map-get? license-templates template-id))

(define-read-only (get-template-stats (template-id uint))
  (map-get? template-stats template-id))

(define-read-only (get-template-counter)
  (var-get template-counter))

(define-read-only (get-template-by-name (owner principal) (name (string-ascii 64)))
  (match (map-get? template-names { owner: owner, name: name })
    name-record (map-get? license-templates (get template-id name-record))
    none))

(define-read-only (get-license-template (license-id uint))
  (map-get? license-template-mapping license-id))

(define-read-only (calculate-template-price (template-id uint) (duration-multiplier uint))
  (match (map-get? license-templates template-id)
    template
      (let ((base-price (get base-price template))
            (duration-type (get duration-type template)))
        (if (is-eq duration-type DURATION_FIXED)
          (ok base-price)
          (if (is-eq duration-type DURATION_RENEWABLE)
            (ok (* base-price duration-multiplier))
            (ok (* base-price u2))))) ;; Perpetual = 2x base price
    ERR_TEMPLATE_NOT_FOUND))

;; Public functions

(define-public (create-template
  (brand-id uint)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (terms-uri (string-utf8 256))
  (duration-type uint)
  (base-duration uint)
  (base-price uint)
  (royalty-percentage uint)
  (usage-rights (string-ascii 256))
  (territory (string-ascii 64)))
  (let ((template-id (+ (var-get template-counter) u1)))
    
    ;; Validate parameters
    (asserts! (> base-price u0) ERR_INVALID_PARAMS)
    (asserts! (<= royalty-percentage u100) ERR_INVALID_PARAMS)
    (asserts! (and (>= duration-type u1) (<= duration-type u3)) ERR_INVALID_PARAMS)
    (asserts! (> base-duration u0) ERR_INVALID_PARAMS)
    
    ;; Check if template name already exists for this owner
    (asserts! (is-none (map-get? template-names { owner: tx-sender, name: name })) ERR_TEMPLATE_NAME_EXISTS)
    
    ;; Verify brand ownership via contract call
    (asserts! (is-ok (contract-call? .brand-rights get-brand-owner brand-id)) ERR_BRAND_NOT_FOUND)
    (asserts! (is-eq tx-sender (unwrap-panic (contract-call? .brand-rights get-brand-owner brand-id))) ERR_NOT_AUTHORIZED)
    
    ;; Create template
    (map-set license-templates template-id
      {
        owner: tx-sender,
        brand-id: brand-id,
        name: name,
        description: description,
        terms-uri: terms-uri,
        duration-type: duration-type,
        base-duration: base-duration,
        base-price: base-price,
        royalty-percentage: royalty-percentage,
        usage-rights: usage-rights,
        territory: territory,
        is-active: true,
        created-at: stacks-block-height,
        updated-at: stacks-block-height
      })
    
    ;; Initialize stats
    (map-set template-stats template-id
      {
        licenses-issued: u0,
        total-revenue: u0,
        last-used: u0,
        avg-duration: base-duration
      })
    
    ;; Register name
    (map-set template-names { owner: tx-sender, name: name } { template-id: template-id })
    
    (var-set template-counter template-id)
    (ok template-id)))

(define-public (set-template-active (template-id uint) (active bool))
  (let ((template (unwrap! (map-get? license-templates template-id) ERR_TEMPLATE_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get owner template)) ERR_NOT_TEMPLATE_OWNER)
    
    (map-set license-templates template-id
      (merge template {
        is-active: active,
        updated-at: stacks-block-height
      }))
    (ok true)))

(define-public (update-template-price (template-id uint) (new-price uint))
  (let ((template (unwrap! (map-get? license-templates template-id) ERR_TEMPLATE_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get owner template)) ERR_NOT_TEMPLATE_OWNER)
    (asserts! (> new-price u0) ERR_INVALID_PARAMS)
    
    (map-set license-templates template-id
      (merge template {
        base-price: new-price,
        updated-at: stacks-block-height
      }))
    (ok true)))

(define-public (issue-license-from-template 
  (template-id uint) 
  (licensee principal) 
  (duration-multiplier uint))
  (let ((template (unwrap! (map-get? license-templates template-id) ERR_TEMPLATE_NOT_FOUND))
        (stats (unwrap! (map-get? template-stats template-id) ERR_TEMPLATE_NOT_FOUND)))
    
    (asserts! (get is-active template) ERR_TEMPLATE_INACTIVE)
    (asserts! (is-eq tx-sender (get owner template)) ERR_NOT_TEMPLATE_OWNER)
    (asserts! (> duration-multiplier u0) ERR_INVALID_PARAMS)
    
    (let ((actual-price (unwrap! (calculate-template-price template-id duration-multiplier) ERR_INVALID_PARAMS))
          (actual-duration (* (get base-duration template) duration-multiplier)))
      
      ;; Issue license via brand-rights contract
      (let ((license-result (contract-call? .brand-rights issue-license
                              (get brand-id template)
                              licensee
                              actual-duration
                              (get usage-rights template)
                              (get royalty-percentage template)
                              (get territory template))))
        
        (match license-result
          license-id
            (begin
              ;; Record template usage
              (map-set license-template-mapping license-id
                {
                  template-id: template-id,
                  issued-at: stacks-block-height,
                  actual-price: actual-price
                })
              
              ;; Update stats
              (let ((new-issued (+ (get licenses-issued stats) u1))
                    (new-revenue (+ (get total-revenue stats) actual-price))
                    (new-avg-duration (/ (+ (* (get avg-duration stats) (get licenses-issued stats)) actual-duration)
                                        new-issued)))
                (map-set template-stats template-id
                  {
                    licenses-issued: new-issued,
                    total-revenue: new-revenue,
                    last-used: stacks-block-height,
                    avg-duration: new-avg-duration
                  }))
              (ok license-id))
          error (err error))))))
