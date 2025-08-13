(define-non-fungible-token brand-license uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-expired (err u104))
(define-constant err-invalid-params (err u105))

(define-data-var next-license-id uint u1)

(define-constant err-renewal-not-configured (err u111))
(define-constant err-not-renewable (err u112))
(define-constant err-renewal-payment-failed (err u113))

(define-map license-renewal-config
  { license-id: uint }
  {
    auto-renew: bool,
    renewal-period: uint,
    renewal-fee: uint,
    max-renewals: uint,
    current-renewals: uint
  }
)

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


(define-map license-usage
  { license-id: uint }
  {
    total-uses: uint,
    last-used: uint,
    usage-limit: (optional uint),
    usage-type: (string-ascii 64)
  }
)

(define-map usage-logs
  { license-id: uint, usage-id: uint }
  {
    used-by: principal,
    used-at: uint,
    usage-amount: uint,
    usage-description: (string-ascii 128)
  }
)

(define-map license-usage-counters
  { license-id: uint }
  { next-usage-id: uint }
)

(define-read-only (get-license-usage (license-id uint))
  (map-get? license-usage { license-id: license-id })
)

(define-read-only (get-usage-log (license-id uint) (usage-id uint))
  (map-get? usage-logs { license-id: license-id, usage-id: usage-id })
)

(define-read-only (get-license-usage-stats (license-id uint))
  (let ((usage (map-get? license-usage { license-id: license-id })))
    (if (is-some usage)
      (ok (unwrap-panic usage))
      (ok { total-uses: u0, last-used: u0, usage-limit: none, usage-type: "" })
    )
  )
)

(define-public (set-usage-limit (license-id uint) (limit uint) (usage-type (string-ascii 64)))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
      
      (map-set license-usage
        { license-id: license-id }
        {
          total-uses: u0,
          last-used: u0,
          usage-limit: (some limit),
          usage-type: usage-type
        }
      )
      
      (map-set license-usage-counters
        { license-id: license-id }
        { next-usage-id: u1 }
      )
      
      (ok true)
    )
  )
)

(define-public (record-usage (license-id uint) (usage-amount uint) (usage-description (string-ascii 128)))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    (let ((license-data (unwrap-panic license)))
      (asserts! (is-eq tx-sender (get licensee license-data)) err-unauthorized)
      (asserts! (is-eq (unwrap! (is-license-valid license-id) err-not-found) true) err-expired)
      
      (let (
        (current-usage (default-to { total-uses: u0, last-used: u0, usage-limit: none, usage-type: "" } 
                                  (map-get? license-usage { license-id: license-id })))
        (usage-counter (default-to { next-usage-id: u1 } 
                                  (map-get? license-usage-counters { license-id: license-id })))
        (usage-id (get next-usage-id usage-counter))
      )
        (match (get usage-limit current-usage)
          limit (asserts! (< (get total-uses current-usage) limit) err-invalid-params)
          true
        )
        
        (map-set usage-logs
          { license-id: license-id, usage-id: usage-id }
          {
            used-by: tx-sender,
            used-at: stacks-block-height,
            usage-amount: usage-amount,
            usage-description: usage-description
          }
        )
        
        (map-set license-usage
          { license-id: license-id }
          (merge current-usage {
            total-uses: (+ (get total-uses current-usage) u1),
            last-used: stacks-block-height
          })
        )
        
        (map-set license-usage-counters
          { license-id: license-id }
          { next-usage-id: (+ usage-id u1) }
        )
        
        (ok usage-id)
      )
    )
  )
)

(define-public (calculate-usage-based-royalty (license-id uint))
  (let (
    (license (map-get? license-details { license-id: license-id }))
    (usage (map-get? license-usage { license-id: license-id }))
  )
    (asserts! (is-some license) err-not-found)
    (asserts! (is-some usage) err-not-found)
    
    (let (
      (license-data (unwrap-panic license))
      (usage-data (unwrap-panic usage))
      (base-royalty (get royalty-percentage license-data))
      (total-uses (get total-uses usage-data))
    )
      (ok (* base-royalty total-uses))
    )
  )
)

(define-constant err-insufficient-approvals (err u108))
(define-constant err-already-approved (err u109))
(define-constant err-not-approver (err u110))

(define-map brand-approvers
  { brand-id: uint }
  {
    approvers: (list 10 principal),
    required-approvals: uint,
    multisig-enabled: bool
  }
)

(define-map pending-licenses
  { pending-id: uint }
  {
    brand-id: uint,
    licensee: principal,
    duration: uint,
    usage-rights: (string-ascii 256),
    royalty-percentage: uint,
    territory: (string-ascii 64),
    approvals: (list 10 principal),
    created-by: principal,
    created-at: uint
  }
)

(define-map pending-transfers
  { pending-id: uint }
  {
    license-id: uint,
    from: principal,
    to: principal,
    approvals: (list 10 principal),
    created-by: principal,
    created-at: uint
  }
)

(define-data-var next-pending-id uint u1)

(define-read-only (get-brand-approvers (brand-id uint))
  (map-get? brand-approvers { brand-id: brand-id })
)

(define-read-only (get-pending-license (pending-id uint))
  (map-get? pending-licenses { pending-id: pending-id })
)

(define-read-only (get-pending-transfer (pending-id uint))
  (map-get? pending-transfers { pending-id: pending-id })
)

(define-public (setup-multisig (brand-id uint) (approvers (list 10 principal)) (required-approvals uint))
  (let ((brand (map-get? brand-details { brand-id: brand-id })))
    (asserts! (is-some brand) err-not-found)
    (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
    (asserts! (> required-approvals u0) err-invalid-params)
    (asserts! (<= required-approvals (len approvers)) err-invalid-params)
    
    (map-set brand-approvers
      { brand-id: brand-id }
      {
        approvers: approvers,
        required-approvals: required-approvals,
        multisig-enabled: true
      }
    )
    
    (ok true)
  )
)

(define-public (propose-license (brand-id uint) (licensee principal) (duration uint) (usage-rights (string-ascii 256)) (royalty-percentage uint) (territory (string-ascii 64)))
  (let (
    (brand (map-get? brand-details { brand-id: brand-id }))
    (multisig (map-get? brand-approvers { brand-id: brand-id }))
    (pending-id (var-get next-pending-id))
  )
    (asserts! (is-some brand) err-not-found)
    (asserts! (is-some multisig) err-not-found)
    (asserts! (get multisig-enabled (unwrap-panic multisig)) err-unauthorized)
    (asserts! (<= royalty-percentage u100) err-invalid-params)
    
    (let ((approvers-list (get approvers (unwrap-panic multisig))))
      (asserts! (is-some (index-of approvers-list tx-sender)) err-not-approver)
      
      (map-set pending-licenses
        { pending-id: pending-id }
        {
          brand-id: brand-id,
          licensee: licensee,
          duration: duration,
          usage-rights: usage-rights,
          royalty-percentage: royalty-percentage,
          territory: territory,
          approvals: (list tx-sender),
          created-by: tx-sender,
          created-at: stacks-block-height
        }
      )
      
      (var-set next-pending-id (+ pending-id u1))
      (ok pending-id)
    )
  )
)

(define-public (approve-license (pending-id uint))
  (let ((pending (map-get? pending-licenses { pending-id: pending-id })))
    (asserts! (is-some pending) err-not-found)
    
    (let (
      (pending-data (unwrap-panic pending))
      (brand-id (get brand-id pending-data))
      (multisig (map-get? brand-approvers { brand-id: brand-id }))
    )
      (asserts! (is-some multisig) err-not-found)
      
      (let (
        (multisig-data (unwrap-panic multisig))
        (approvers-list (get approvers multisig-data))
        (current-approvals (get approvals pending-data))
      )
        (asserts! (is-some (index-of approvers-list tx-sender)) err-not-approver)
        (asserts! (is-none (index-of current-approvals tx-sender)) err-already-approved)
        
        (let ((new-approvals (unwrap-panic (as-max-len? (append current-approvals tx-sender) u10))))
          (map-set pending-licenses
            { pending-id: pending-id }
            (merge pending-data { approvals: new-approvals })
          )
          
          (if (>= (len new-approvals) (get required-approvals multisig-data))
            (execute-license-approval pending-id)
            (ok pending-id)
          )
        )
      )
    )
  )
)
(define-private (execute-license-approval (pending-id uint))
  (let ((pending (map-get? pending-licenses { pending-id: pending-id })))
    (asserts! (is-some pending) err-not-found)
    
    (let (
      (pending-data (unwrap-panic pending))
      (license-id (var-get next-license-id))
    )
      (try! (nft-mint? brand-license license-id (get licensee pending-data)))
      
      (map-set license-details
        { license-id: license-id }
        {
          brand-id: (get brand-id pending-data),
          licensee: (get licensee pending-data),
          issued-at: stacks-block-height,
          expires-at: (+ stacks-block-height (get duration pending-data)),
          usage-rights: (get usage-rights pending-data),
          royalty-percentage: (get royalty-percentage pending-data),
          territory: (get territory pending-data),
          revoked: false
        }
      )
      
      (var-set next-license-id (+ license-id u1))
      (map-delete pending-licenses { pending-id: pending-id })
      
      (ok license-id)
    )
  )
)

(define-public (propose-transfer (license-id uint) (recipient principal))
  (let (
    (license (map-get? license-details { license-id: license-id }))
    (owner (unwrap! (nft-get-owner? brand-license license-id) err-not-found))
    (pending-id (var-get next-pending-id))
  )
    (asserts! (is-some license) err-not-found)
    (asserts! (is-eq tx-sender owner) err-unauthorized)
    
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (multisig (map-get? brand-approvers { brand-id: brand-id }))
    )
      (if (and (is-some multisig) (get multisig-enabled (unwrap-panic multisig)))
        (begin
          (map-set pending-transfers
            { pending-id: pending-id }
            {
              license-id: license-id,
              from: tx-sender,
              to: recipient,
              approvals: (list tx-sender),
              created-by: tx-sender,
              created-at: stacks-block-height
            }
          )
          (var-set next-pending-id (+ pending-id u1))
          (ok pending-id)
        )
        (begin
          (try! (nft-transfer? brand-license license-id tx-sender recipient))
          (map-set license-details
            { license-id: license-id }
            (merge license-data { licensee: recipient })
          )
          (ok u0)
        )
      )
    )
  )
)



(define-read-only (get-renewal-config (license-id uint))
  (map-get? license-renewal-config { license-id: license-id })
)

(define-read-only (can-renew-license (license-id uint))
  (let (
    (license (map-get? license-details { license-id: license-id }))
    (config (map-get? license-renewal-config { license-id: license-id }))
  )
    (if (and (is-some license) (is-some config))
      (let (
        (license-data (unwrap-panic license))
        (config-data (unwrap-panic config))
      )
        (if (get revoked license-data)
          (ok false)
          (if (> (get max-renewals config-data) (get current-renewals config-data))
            (ok true)
            (ok false)
          )
        )
      )
      (ok false)
    )
  )
)

(define-public (configure-renewal (license-id uint) (auto-renew bool) (renewal-period uint) (renewal-fee uint) (max-renewals uint))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
      (asserts! (> renewal-period u0) err-invalid-params)
      (asserts! (> max-renewals u0) err-invalid-params)
      
      (map-set license-renewal-config
        { license-id: license-id }
        {
          auto-renew: auto-renew,
          renewal-period: renewal-period,
          renewal-fee: renewal-fee,
          max-renewals: max-renewals,
          current-renewals: u0
        }
      )
      
      (ok true)
    )
  )
)

(define-public (renew-license (license-id uint))
  (let (
    (license (map-get? license-details { license-id: license-id }))
    (config (map-get? license-renewal-config { license-id: license-id }))
  )
    (asserts! (is-some license) err-not-found)
    (asserts! (is-some config) err-renewal-not-configured)
    
    (let (
      (license-data (unwrap-panic license))
      (config-data (unwrap-panic config))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (asserts! (not (get revoked license-data)) err-expired)
      (asserts! (> (get max-renewals config-data) (get current-renewals config-data)) err-not-renewable)
      
      (let (
        (brand-owner (get owner (unwrap-panic brand)))
        (renewal-fee (get renewal-fee config-data))
        (renewal-period (get renewal-period config-data))
        (current-expiry (get expires-at license-data))
        (new-expiry (+ current-expiry renewal-period))
      )
        (if (> renewal-fee u0)
          (try! (stx-transfer? renewal-fee tx-sender brand-owner))
          true
        )
        
        (map-set license-details
          { license-id: license-id }
          (merge license-data { expires-at: new-expiry })
        )
        
        (map-set license-renewal-config
          { license-id: license-id }
          (merge config-data { current-renewals: (+ (get current-renewals config-data) u1) })
        )
        
        (if (> renewal-fee u0)
          (begin
            (map-set brand-royalties
              { brand-id: brand-id }
              { royalties-collected: (+ (get-royalties-collected brand-id) renewal-fee) }
            )
            (let ((current-paid (default-to { total-paid: u0 } (map-get? licensee-payments { licensee: tx-sender, brand-id: brand-id }))))
              (map-set licensee-payments
                { licensee: tx-sender, brand-id: brand-id }
                { total-paid: (+ (get total-paid current-paid) renewal-fee) }
              )
            )
          )
          true
        )
        
        (ok new-expiry)
      )
    )
  )
)

(define-public (auto-renew-license (license-id uint))
  (let (
    (license (map-get? license-details { license-id: license-id }))
    (config (map-get? license-renewal-config { license-id: license-id }))
  )
    (asserts! (is-some license) err-not-found)
    (asserts! (is-some config) err-renewal-not-configured)
    
    (let (
      (license-data (unwrap-panic license))
      (config-data (unwrap-panic config))
    )
      (asserts! (get auto-renew config-data) err-not-renewable)
      (asserts! (is-eq tx-sender (get licensee license-data)) err-unauthorized)
      (asserts! (<= (get expires-at license-data) (+ stacks-block-height u144)) err-invalid-params)
      
      (renew-license license-id)
    )
  )
)

(define-public (disable-auto-renewal (license-id uint))
  (let (
    (license (map-get? license-details { license-id: license-id }))
    (config (map-get? license-renewal-config { license-id: license-id }))
  )
    (asserts! (is-some license) err-not-found)
    (asserts! (is-some config) err-renewal-not-configured)
    
    (let (
      (license-data (unwrap-panic license))
      (config-data (unwrap-panic config))
    )
      (asserts! (is-eq tx-sender (get licensee license-data)) err-unauthorized)
      
      (map-set license-renewal-config
        { license-id: license-id }
        (merge config-data { auto-renew: false })
      )
      
      (ok true)
    )
  )
)

;; License Verification & Authenticity System
;; Provides third-party verification capabilities for license authenticity

(define-constant err-verification-not-found (err u114))
(define-constant err-verification-expired (err u115))
(define-constant err-invalid-verification-token (err u116))
(define-constant err-verification-disabled (err u117))

;; Counter for unique verification tokens
(define-data-var next-verification-id uint u1)

;; Store verification tokens for each license
(define-map license-verification-tokens
  { license-id: uint }
  {
    verification-token: uint,
    token-created-at: uint,
    verification-enabled: bool,
    public-verification: bool,
    verification-count: uint
  }
)

;; Track verification attempts and history
(define-map verification-attempts
  { verification-token: uint, attempt-id: uint }
  {
    verifier: principal,
    verified-at: uint,
    verification-result: bool,
    license-id: uint,
    verification-details: (string-ascii 128)
  }
)

;; Counter for verification attempts per token
(define-map verification-counters
  { verification-token: uint }
  { next-attempt-id: uint }
)

;; Verification settings per brand
(define-map brand-verification-settings
  { brand-id: uint }
  {
    require-verification: bool,
    public-verification-allowed: bool,
    verification-fee: uint,
    max-verifications-per-day: uint
  }
)

;; Daily verification tracking
(define-map daily-verification-count
  { verification-token: uint, day: uint }
  { count: uint }
)

;; Read-only function to get verification token details
(define-read-only (get-verification-token (license-id uint))
  (map-get? license-verification-tokens { license-id: license-id })
)

;; Read-only function to get brand verification settings
(define-read-only (get-brand-verification-settings (brand-id uint))
  (map-get? brand-verification-settings { brand-id: brand-id })
)

;; Read-only function to get verification attempt details
(define-read-only (get-verification-attempt (verification-token uint) (attempt-id uint))
  (map-get? verification-attempts { verification-token: verification-token, attempt-id: attempt-id })
)

;; Map to store verification token to license mappings for efficient lookup
(define-map token-to-license
  { verification-token: uint }
  { license-id: uint }
)

;; Public function to verify license authenticity by token
(define-read-only (verify-license-by-token (verification-token uint))
  (let ((token-mapping (map-get? token-to-license { verification-token: verification-token })))
    (if (is-some token-mapping)
      (let (
        (license-id (get license-id (unwrap-panic token-mapping)))
        (license (map-get? license-details { license-id: license-id }))
        (token-data (map-get? license-verification-tokens { license-id: license-id }))
      )
        (if (and (is-some license) (is-some token-data))
          (let (
            (license-data (unwrap-panic license))
            (token-info (unwrap-panic token-data))
          )
            (if (not (get verification-enabled token-info))
              err-verification-disabled
              (if (get revoked license-data)
                (ok { valid: false, reason: "license-revoked", license-id: license-id })
                (if (> (get expires-at license-data) stacks-block-height)
                  (ok { valid: true, reason: "license-active", license-id: license-id })
                  (ok { valid: false, reason: "license-expired", license-id: license-id })
                )
              )
            )
          )
          err-verification-not-found
        )
      )
      err-invalid-verification-token
    )
  )
)

;; Configure verification settings for a brand
(define-public (configure-brand-verification (brand-id uint) (require-verification bool) (public-verification-allowed bool) (verification-fee uint) (max-verifications-per-day uint))
  (let ((brand (map-get? brand-details { brand-id: brand-id })))
    (asserts! (is-some brand) err-not-found)
    (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
    
    (map-set brand-verification-settings
      { brand-id: brand-id }
      {
        require-verification: require-verification,
        public-verification-allowed: public-verification-allowed,
        verification-fee: verification-fee,
        max-verifications-per-day: max-verifications-per-day
      }
    )
    
    (ok true)
  )
)

;; Generate verification token for a license
(define-public (generate-verification-token (license-id uint) (public-verification bool))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
      
      (let ((verification-token (var-get next-verification-id)))
        (map-set license-verification-tokens
          { license-id: license-id }
          {
            verification-token: verification-token,
            token-created-at: stacks-block-height,
            verification-enabled: true,
            public-verification: public-verification,
            verification-count: u0
          }
        )
        
        ;; Store token-to-license mapping for efficient lookup
        (map-set token-to-license
          { verification-token: verification-token }
          { license-id: license-id }
        )
        
        (map-set verification-counters
          { verification-token: verification-token }
          { next-attempt-id: u1 }
        )
        
        (var-set next-verification-id (+ verification-token u1))
        (ok verification-token)
      )
    )
  )
)

;; Perform verification attempt with detailed logging
(define-public (perform-verification (verification-token uint) (verification-details (string-ascii 128)))
  (let ((token-mapping (map-get? token-to-license { verification-token: verification-token })))
    (asserts! (is-some token-mapping) err-invalid-verification-token)
    
    (let (
      (license-id (get license-id (unwrap-panic token-mapping)))
      (license (unwrap! (map-get? license-details { license-id: license-id }) err-not-found))
      (token-data (unwrap! (map-get? license-verification-tokens { license-id: license-id }) err-verification-not-found))
      (counter-data (default-to { next-attempt-id: u1 } (map-get? verification-counters { verification-token: verification-token })))
      (attempt-id (get next-attempt-id counter-data))
    )
      (asserts! (get verification-enabled token-data) err-verification-disabled)
      
      (let (
        (license-data license)
        (is-valid (and (not (get revoked license-data)) (> (get expires-at license-data) stacks-block-height)))
        (current-day (/ stacks-block-height u144))
        (daily-count (default-to { count: u0 } (map-get? daily-verification-count { verification-token: verification-token, day: current-day })))
        (brand-settings (map-get? brand-verification-settings { brand-id: (get brand-id license-data) }))
      )
        ;; Check brand verification settings
        (if (is-some brand-settings)
          (let ((settings (unwrap-panic brand-settings)))
            (asserts! (< (get count daily-count) (get max-verifications-per-day settings)) err-invalid-params)
            
            ;; Check if public verification is allowed
            (if (not (get public-verification token-data))
              (asserts! (get public-verification-allowed settings) err-unauthorized)
              true
            )
            
            ;; Process verification fee if required
            (if (> (get verification-fee settings) u0)
              (let ((brand-data (unwrap! (map-get? brand-details { brand-id: (get brand-id license-data) }) err-not-found)))
                (try! (stx-transfer? (get verification-fee settings) tx-sender (get owner brand-data)))
              )
              true
            )
          )
          true
        )
        
        ;; Log verification attempt
        (map-set verification-attempts
          { verification-token: verification-token, attempt-id: attempt-id }
          {
            verifier: tx-sender,
            verified-at: stacks-block-height,
            verification-result: is-valid,
            license-id: license-id,
            verification-details: verification-details
          }
        )
        
        ;; Update counters
        (map-set verification-counters
          { verification-token: verification-token }
          { next-attempt-id: (+ attempt-id u1) }
        )
        
        (map-set license-verification-tokens
          { license-id: license-id }
          (merge token-data { verification-count: (+ (get verification-count token-data) u1) })
        )
        
        (map-set daily-verification-count
          { verification-token: verification-token, day: current-day }
          { count: (+ (get count daily-count) u1) }
        )
        
        (ok { 
          verification-result: {
            valid: is-valid,
            reason: (if is-valid "license-active" (if (get revoked license-data) "license-revoked" "license-expired")),
            license-id: license-id
          },
          attempt-id: attempt-id 
        })
      )
    )
  )
)

;; Disable verification for a license
(define-public (disable-verification (license-id uint))
  (let ((license (map-get? license-details { license-id: license-id })))
    (asserts! (is-some license) err-not-found)
    
    (let (
      (license-data (unwrap-panic license))
      (brand-id (get brand-id license-data))
      (brand (map-get? brand-details { brand-id: brand-id }))
      (token-data (map-get? license-verification-tokens { license-id: license-id }))
    )
      (asserts! (is-some brand) err-not-found)
      (asserts! (is-some token-data) err-verification-not-found)
      (asserts! (is-eq tx-sender (get owner (unwrap-panic brand))) err-owner-only)
      
      (map-set license-verification-tokens
        { license-id: license-id }
        (merge (unwrap-panic token-data) { verification-enabled: false })
      )
      
      (ok true)
    )
  )
)

;; Get verification statistics for a license
(define-read-only (get-verification-stats (license-id uint))
  (let ((token-data (map-get? license-verification-tokens { license-id: license-id })))
    (if (is-some token-data)
      (ok (unwrap-panic token-data))
      err-verification-not-found
    )
  )
)


