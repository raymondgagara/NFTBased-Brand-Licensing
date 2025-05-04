(define-non-fungible-token brand-license uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-expired (err u104))
(define-constant err-invalid-params (err u105))

(define-data-var next-license-id uint u1)

(define-map brand-details
  { brand-id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    owner: principal,
    created-at: uint,
    metadata-uri: (string-utf8 256)
  }
)

(define-map license-details
  { license-id: uint }
  {
    brand-id: uint,
    licensee: principal,
    issued-at: uint,
    expires-at: uint,
    usage-rights: (string-ascii 256),
    royalty-percentage: uint,
    territory: (string-ascii 64),
    revoked: bool
  }
)

(define-map brand-royalties
  { brand-id: uint }
  { royalties-collected: uint }
)

(define-map licensee-payments
  { licensee: principal, brand-id: uint }
  { total-paid: uint }
)

(define-read-only (get-brand-details (brand-id uint))
  (map-get? brand-details { brand-id: brand-id })
)

(define-read-only (get-license-details (license-id uint))
  (map-get? license-details { license-id: license-id })
)

(define-read-only (get-license-owner (license-id uint))
  (let ((license (map-get? license-details { license-id: license-id })))
    (if (is-some license)
      (ok (get licensee (unwrap-panic license)))
      err-not-found
    )
  )
)

(define-read-only (get-brand-owner (brand-id uint))
  (let ((brand (map-get? brand-details { brand-id: brand-id })))
    (if (is-some brand)
      (ok (get owner (unwrap-panic brand)))
      err-not-found
    )
  )
)

(define-read-only (is-license-valid (license-id uint))
  (let ((license (map-get? license-details { license-id: license-id })))
    (if (is-some license)
      (let ((license-data (unwrap-panic license)))
        (if (get revoked license-data)
          (ok false)
          (if (> (get expires-at license-data) stacks-block-height)
            (ok true)
            (ok false)
          )
        )
      )
      err-not-found
    )
  )
)

(define-read-only (get-royalties-collected (brand-id uint))
  (default-to u0 (get royalties-collected (map-get? brand-royalties { brand-id: brand-id })))
)

(define-public (register-brand (name (string-ascii 64)) (description (string-ascii 256)) (metadata-uri (string-utf8 256)))
  (let ((brand-id (var-get next-license-id)))
    (map-set brand-details
      { brand-id: brand-id }
      {
        name: name,
        description: description,
        owner: tx-sender,
        created-at: stacks-block-height,
        metadata-uri: metadata-uri
      }
    )
    (map-set brand-royalties
      { brand-id: brand-id }
      { royalties-collected: u0 }
    )
    (var-set next-license-id (+ brand-id u1))
    (ok brand-id)
  )
)

(define-public (issue-license (brand-id uint) (licensee principal) (duration uint) (usage-rights (string-ascii 256)) (royalty-percentage uint) (territory (string-ascii 64)))
  (let (
    (brand (map-get? brand-details { brand-id: brand-id }))
    (license-id (var-get next-license-id))
  )
    (asserts! (is-some brand) err-not-found)
    (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
    (asserts! (<= royalty-percentage u100) err-invalid-params)
    
    (try! (nft-mint? brand-license license-id licensee))
    
    (map-set license-details
      { license-id: license-id }
      {
        brand-id: brand-id,
        licensee: licensee,
        issued-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration),
        usage-rights: usage-rights,
        royalty-percentage: royalty-percentage,
        territory: territory,
        revoked: false
      }
    )
    
    (var-set next-license-id (+ license-id u1))
    (ok license-id)
  )
)

(define-public (revoke-license (license-id uint))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
      
      (map-set license-details
        { license-id: license-id }
        (merge license-data { revoked: true })
      )
      
      (ok true)
    )
  )
)

(define-public (pay-royalty (license-id uint) (amount uint))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (let ((brand-owner (get owner (unwrap-panic brand))))
        (try! (stx-transfer? amount tx-sender brand-owner))
        
        (map-set brand-royalties
          { brand-id: brand-id }
          { royalties-collected: (+ (get-royalties-collected brand-id) amount) }
        )
        
        (let ((current-paid (default-to { total-paid: u0 } (map-get? licensee-payments { licensee: tx-sender, brand-id: brand-id }))))
          (map-set licensee-payments
            { licensee: tx-sender, brand-id: brand-id }
            { total-paid: (+ (get total-paid current-paid) amount) }
          )
        )
        
        (ok true)
      )
    )
  )
)

(define-public (transfer-license (license-id uint) (recipient principal))
  (let ((owner (unwrap! (nft-get-owner? brand-license license-id) err-not-found)))
    (asserts! (is-eq tx-sender owner) err-unauthorized)
    (try! (nft-transfer? brand-license license-id tx-sender recipient))
    
    (let ((license (map-get? license-details { license-id: license-id })))
      (asserts! (is-some license) err-not-found)
      (map-set license-details
        { license-id: license-id }
        (merge (unwrap-panic license) { licensee: recipient })
      )
      (ok true)
    )
  )
)

(define-public (extend-license (license-id uint) (additional-duration uint))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
      
      (map-set license-details
        { license-id: license-id }
        (merge license-data { expires-at: (+ (get expires-at license-data) additional-duration) })
      )
      
      (ok true)
    )
  )
)


(define-constant err-not-for-sale (err u106))
(define-constant err-price-not-met (err u107))

;; Add this map to track listings
(define-map license-listings
  { license-id: uint }
  { 
    price: uint,
    seller: principal
  }
)

;; Read-only function to check if a license is for sale
(define-read-only (get-license-listing (license-id uint))
  (map-get? license-listings { license-id: license-id })
)

;; List a license for sale
(define-public (list-license-for-sale (license-id uint) (price uint))
  (let ((owner (unwrap! (nft-get-owner? brand-license license-id) err-not-found)))
    (asserts! (is-eq tx-sender owner) err-unauthorized)
    (asserts! (> price u0) err-invalid-params)
    
    ;; Check if license is valid
    (asserts! (is-eq (unwrap! (is-license-valid license-id) err-not-found) true) err-expired)
    
    (map-set license-listings
      { license-id: license-id }
      { 
        price: price,
        seller: tx-sender
      }
    )
    (ok true)
  )
)

;; Cancel a listing
(define-public (cancel-license-listing (license-id uint))
  (let ((listing (map-get? license-listings { license-id: license-id })))
    (asserts! (is-some listing) err-not-found)
    (asserts! (is-eq tx-sender (get seller (unwrap-panic listing))) err-unauthorized)
    
    (map-delete license-listings { license-id: license-id })
    (ok true)
  )
)

;; Purchase a listed license
(define-public (purchase-license (license-id uint))
  (let ((listing (map-get? license-listings { license-id: license-id })))
    (asserts! (is-some listing) err-not-for-sale)
    
    (let ((listing-data (unwrap-panic listing))
          (price (get price listing-data))
          (seller (get seller listing-data)))
      
      ;; Transfer STX from buyer to seller
      (try! (stx-transfer? price tx-sender seller))
      
      ;; Transfer the NFT
      (try! (nft-transfer? brand-license license-id seller tx-sender))
      
      ;; Update license details
      (let ((license (map-get? license-details { license-id: license-id })))
        (asserts! (is-some license) err-not-found)
        (map-set license-details
          { license-id: license-id }
          (merge (unwrap-panic license) { licensee: tx-sender })
        )
      )
      
      ;; Remove the listing
      (map-delete license-listings { license-id: license-id })
      
      (ok true)
    )
  )
)


(define-public (get-licensee-payments (licensee principal) (brand-id uint))
  (let ((payments (map-get? licensee-payments { licensee: licensee, brand-id: brand-id })))
    (if (is-some payments)
      (ok (get total-paid (unwrap-panic payments)))
      (ok u0)
    )
  )
)
(define-public (get-brand-royalties (brand-id uint))
  (let ((royalties (map-get? brand-royalties { brand-id: brand-id })))
    (if (is-some royalties)
      (ok (get royalties-collected (unwrap-panic royalties)))
      (ok u0)
    )
  )
)
