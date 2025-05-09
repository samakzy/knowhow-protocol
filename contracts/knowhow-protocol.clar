;; KnowHow - P2P Expertise Protocol
;; This smart contract enables users to exchange time-based expertise for cryptocurrency
;; Built on Stacks blockchain using Clarity language

;; ========================================
;; GLOBAL PARAMETERS
;; ========================================

;; Base rate for expertise registration (in microstacks per hour)
(define-data-var base-hourly-rate uint u10)

;; Maximum expertise hours a single user can register
(define-data-var user-expertise-limit uint u100)

;; Platform fee percentage (out of 100)
(define-data-var platform-fee-percentage uint u10)

;; Current total expertise hours in the marketplace
(define-data-var marketplace-hour-volume uint u0)

;; Global cap on expertise hours in the marketplace
(define-data-var marketplace-hour-cap uint u1000)

;; ========================================
;; ERROR CODES
;; ========================================

(define-constant error-insufficient-funds (err u201))
(define-constant error-invalid-hours (err u202))
(define-constant error-invalid-price (err u203))
(define-constant error-market-capacity (err u204))
(define-constant error-self-transaction (err u205))
(define-constant error-positive-required (err u206))
(define-constant error-exceeds-limit (err u207))
(define-constant error-rating-too-low (err u212))
(define-constant error-rating-too-high (err u213))
(define-constant error-discount-too-low (err u214))
(define-constant error-discount-too-high (err u215))

;; ========================================
;; DATA STRUCTURES
;; ========================================

;; User funds balance in the platform (in microstacks)
(define-map user-funds-balance principal uint)

;; User expertise hours balance
(define-map user-expertise-balance principal uint)

;; Marketplace listing of expertise hours with associated rates
(define-map marketplace-listings {expert: principal} {hours: uint, price-per-hour: uint})

;; Marketplace listings for verified experts
(define-map verified-listings {expert: principal} {hours: uint, price-per-hour: uint, verification-status: bool})

;; Discounted expertise packages
(define-map expertise-packages {expert: principal} {hours: uint, price-per-hour: uint, discount-rate: uint})

;; Expert verification registry
(define-map expert-verification principal bool)

;; Reputation system
(define-map expert-feedback {expert: principal, reviewer: principal} uint)
(define-map expert-reputation principal {cumulative-score: uint, review-count: uint})

;; Collaborative expertise sharing sessions
(define-map collaborative-sessions uint {organizer: principal, members: (list 10 principal), hours-per-member: uint, price-per-hour: uint, session-status: (string-ascii 20)})
(define-data-var session-counter uint u0)

;; ========================================
;; ADMINISTRATIVE CONFIGURATION
;; ========================================

;; Contract administrator
(define-constant admin-address tx-sender)

;; Error codes for administrative functions
(define-constant error-admin-only (err u200))
(define-constant error-param-invalid (err u208))
(define-constant error-max-too-small (err u209))
(define-constant error-limit-violation (err u210))


;; ========================================
;; PRIVATE FUNCTIONS
;; ========================================

;; Calculate platform's commission from transaction
(define-private (calculate-commission (transaction-amount uint))
  (/ (* transaction-amount (var-get platform-fee-percentage)) u100))

;; Update marketplace hour volume accounting
(define-private (update-marketplace-volume (hour-change int))
  (let (
    (current-volume (var-get marketplace-hour-volume))
    (new-volume (if (< hour-change 0)
                     (if (>= current-volume (to-uint (- 0 hour-change)))
                         (- current-volume (to-uint (- 0 hour-change)))
                         u0)
                     (+ current-volume (to-uint hour-change))))
  )
    (asserts! (<= new-volume (var-get marketplace-hour-cap)) error-market-capacity)
    (var-set marketplace-hour-volume new-volume)
    (ok true)))

;; ========================================
;; USER FUNDS MANAGEMENT
;; ========================================

;; Add funds to user's platform balance
(define-public (deposit-funds (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-funds-balance tx-sender)))
    (new-balance (+ current-balance amount))
  )
    (asserts! (> amount u0) error-positive-required)
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update user's funds balance in the platform
    (map-set user-funds-balance tx-sender new-balance)
    (ok true)))

;; Withdraw funds from user's platform balance
(define-public (withdraw-funds (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-funds-balance tx-sender)))
    (contract-balance (as-contract (stx-get-balance tx-sender)))
  )
    (asserts! (> amount u0) error-positive-required)
    (asserts! (>= current-balance amount) error-insufficient-funds)
    (asserts! (>= contract-balance amount) error-insufficient-funds)

    ;; Transfer STX from contract to user
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    ;; Update user's funds balance in the platform
    (map-set user-funds-balance tx-sender (- current-balance amount))

    (ok true)))

;; ========================================
;; EXPERTISE REGISTRATION AND MANAGEMENT
;; ========================================

;; Register new expertise hours to user's account
(define-public (register-expertise-hours (hours uint))
  (let (
    (requester tx-sender)
    (current-balance (default-to u0 (map-get? user-expertise-balance requester)))
    (max-allowed (var-get user-expertise-limit))
    (cost-per-hour (var-get base-hourly-rate))
    (total-cost (* hours cost-per-hour))
    (available-funds (default-to u0 (map-get? user-funds-balance requester)))
  )
    (asserts! (> hours u0) error-invalid-hours)
    (asserts! (<= (+ current-balance hours) max-allowed) error-exceeds-limit)
    (asserts! (>= available-funds total-cost) error-insufficient-funds)

    ;; Update user expertise balance
    (map-set user-expertise-balance requester (+ current-balance hours))

    ;; Update user funds balance
    (map-set user-funds-balance requester (- available-funds total-cost))

    ;; Update admin balance with registration fee
    (map-set user-funds-balance admin-address (+ (default-to u0 (map-get? user-funds-balance admin-address)) total-cost))

    (ok true)))

;; List expertise hours on the marketplace
(define-public (list-expertise-hours (hours uint) (price-per-hour uint))
  (let (
    (current-balance (default-to u0 (map-get? user-expertise-balance tx-sender)))
    (current-listing (get hours (default-to {hours: u0, price-per-hour: u0} (map-get? marketplace-listings {expert: tx-sender}))))
    (new-total-listed (+ hours current-listing))
  )
    (asserts! (> hours u0) error-invalid-hours)
    (asserts! (> price-per-hour u0) error-invalid-price)
    (asserts! (>= current-balance new-total-listed) error-insufficient-funds)
    (try! (update-marketplace-volume (to-int hours)))
    (map-set marketplace-listings {expert: tx-sender} {hours: new-total-listed, price-per-hour: price-per-hour})
    (ok true)))

;; Withdraw expertise hours from marketplace listing
(define-public (withdraw-listed-hours (hours uint))
  (let (
    (listing-data (default-to {hours: u0, price-per-hour: u0} (map-get? marketplace-listings {expert: tx-sender})))
    (available-hours (get hours listing-data))
    (user-balance (default-to u0 (map-get? user-expertise-balance tx-sender)))
  )
    (asserts! (> hours u0) error-invalid-hours)
    (asserts! (>= available-hours hours) error-insufficient-funds)

    ;; Update listed expertise hours
    (map-set marketplace-listings {expert: tx-sender} {
      hours: (- available-hours hours),
      price-per-hour: (get price-per-hour listing-data)
    })

    ;; Maintain user's expertise balance
    (map-set user-expertise-balance tx-sender user-balance)

    ;; Check and update verified listings if applicable
    (if (is-some (map-get? verified-listings {expert: tx-sender}))
        (let (
          (verified-data (unwrap-panic (map-get? verified-listings {expert: tx-sender})))
          (verified-hours (get hours verified-data))
        )
          (if (>= verified-hours hours)
              (map-set verified-listings {expert: tx-sender} {
                hours: (- verified-hours hours),
                price-per-hour: (get price-per-hour verified-data),
                verification-status: (get verification-status verified-data)
              })
              (map-delete verified-listings {expert: tx-sender})
          )
        )
        true
    )

    (ok true)))

;; ========================================
;; EXPERTISE EXCHANGE
;; ========================================

;; Purchase expertise hours from a provider
(define-public (purchase-expertise (provider principal) (hours uint))
  (let (
    (listing-data (default-to {hours: u0, price-per-hour: u0} (map-get? marketplace-listings {expert: provider})))
    (transaction-amount (* hours (get price-per-hour listing-data)))
    (platform-commission (calculate-commission transaction-amount))
    (total-cost (+ transaction-amount platform-commission))
    (provider-expertise (default-to u0 (map-get? user-expertise-balance provider)))
    (buyer-funds (default-to u0 (map-get? user-funds-balance tx-sender)))
    (provider-funds (default-to u0 (map-get? user-funds-balance provider)))
  )
    (asserts! (not (is-eq tx-sender provider)) error-self-transaction)
    (asserts! (> hours u0) error-invalid-hours)
    (asserts! (>= (get hours listing-data) hours) error-insufficient-funds)
    (asserts! (>= provider-expertise hours) error-insufficient-funds)
    (asserts! (>= buyer-funds total-cost) error-insufficient-funds)

    ;; Update provider's expertise balance and listing
    (map-set user-expertise-balance provider (- provider-expertise hours))
    (map-set marketplace-listings {expert: provider} 
             {hours: (- (get hours listing-data) hours), price-per-hour: (get price-per-hour listing-data)})

    ;; Update buyer's funds and expertise balance
    (map-set user-funds-balance tx-sender (- buyer-funds total-cost))
    (map-set user-expertise-balance tx-sender (+ (default-to u0 (map-get? user-expertise-balance tx-sender)) hours))

    ;; Update provider's funds balance
    (map-set user-funds-balance provider (+ provider-funds transaction-amount))

    ;; Transfer platform commission to admin
    (map-set user-funds-balance admin-address (+ (default-to u0 (map-get? user-funds-balance admin-address)) platform-commission))

    (ok true)))

;; ========================================
;; PREMIUM VERIFIED EXPERTISE
;; ========================================

;; List verified premium expertise hours
(define-public (list-verified-expertise (hours uint) (price-per-hour uint))
  (let (
    (current-balance (default-to u0 (map-get? user-expertise-balance tx-sender)))
    (verification-status (default-to false (map-get? expert-verification tx-sender)))
    (current-listing (get hours (default-to {hours: u0, price-per-hour: u0} (map-get? marketplace-listings {expert: tx-sender}))))
    (new-total-listed (+ hours current-listing))
  )
    (asserts! (> hours u0) error-invalid-hours)
    (asserts! (> price-per-hour u0) error-invalid-price)
    (asserts! verification-status (err u211))
    (asserts! (>= current-balance new-total-listed) error-insufficient-funds)
    (try! (update-marketplace-volume (to-int hours)))

    ;; Update regular marketplace listings
    (map-set marketplace-listings {expert: tx-sender} {hours: new-total-listed, price-per-hour: price-per-hour})

    ;; Add to verified expertise listings
    (map-set verified-listings {expert: tx-sender} {hours: hours, price-per-hour: price-per-hour, verification-status: true})

    (ok true)))

;; ========================================
;; REPUTATION SYSTEM
;; ========================================

;; Rate an expertise provider after exchange
(define-public (rate-provider (provider principal) (rating uint))
  (let (
    (provider-data (default-to {cumulative-score: u0, review-count: u0} (map-get? expert-reputation provider)))
    (current-score (get cumulative-score provider-data))
    (current-count (get review-count provider-data))
    (new-score (+ current-score rating))
    (new-count (+ current-count u1))
  )
    (asserts! (not (is-eq tx-sender provider)) error-self-transaction)
    (asserts! (>= rating u1) error-rating-too-low)
    (asserts! (<= rating u5) error-rating-too-high)

    ;; Update provider's reputation data
    (map-set expert-feedback {expert: provider, reviewer: tx-sender} rating)
    (map-set expert-reputation provider {cumulative-score: new-score, review-count: new-count})

    (ok true)))

;; ========================================
;; DISCOUNTED PACKAGES
;; ========================================

;; Create a discounted expertise package
(define-public (create-expertise-package (hours uint) (price-per-hour uint) (discount-rate uint))
  (let (
    (current-balance (default-to u0 (map-get? user-expertise-balance tx-sender)))
    (current-listing (get hours (default-to {hours: u0, price-per-hour: u0} (map-get? marketplace-listings {expert: tx-sender}))))
    (current-package (default-to {hours: u0, price-per-hour: u0, discount-rate: u0} (map-get? expertise-packages {expert: tx-sender})))
    (new-total-listed (+ hours current-listing))
    (total-package-hours (+ hours (get hours current-package)))
  )
    (asserts! (> hours u0) error-invalid-hours)
    (asserts! (> price-per-hour u0) error-invalid-price)
    (asserts! (> discount-rate u0) error-discount-too-low)
    (asserts! (<= discount-rate u50) error-discount-too-high)
    (asserts! (>= current-balance new-total-listed) error-insufficient-funds)

    ;; Update marketplace volume
    (try! (update-marketplace-volume (to-int hours)))

    ;; Update marketplace listings
    (map-set marketplace-listings {expert: tx-sender} {hours: new-total-listed, price-per-hour: price-per-hour})

    ;; Create or update package
    (map-set expertise-packages {expert: tx-sender} {hours: total-package-hours, price-per-hour: price-per-hour, discount-rate: discount-rate})

    (ok true)))

;; ========================================
;; COLLABORATIVE SESSIONS
;; ========================================

;; Create a collaborative expertise session
(define-public (create-collaborative-session (members (list 10 principal)) (hours-per-member uint) (price-per-hour uint))
  (let (
    (current-balance (default-to u0 (map-get? user-expertise-balance tx-sender)))
    (session-id (var-get session-counter))
    (member-count (len members))
    (total-required-hours (* hours-per-member member-count))
  )
    (asserts! (> hours-per-member u0) error-invalid-hours)
    (asserts! (> price-per-hour u0) error-invalid-price)
    (asserts! (>= current-balance total-required-hours) error-insufficient-funds)

    ;; Update marketplace volume
    (try! (update-marketplace-volume (to-int total-required-hours)))

    ;; Update organizer's expertise balance
    (map-set user-expertise-balance tx-sender (- current-balance total-required-hours))

    ;; Increment session counter
    (var-set session-counter (+ session-id u1))

    (ok session-id)))

;; ========================================
;; ADMINISTRATIVE FUNCTIONS
;; ========================================

;; Update platform configuration parameters
(define-public (update-platform-configuration (new-rate uint) (new-fee-percentage uint) (new-user-limit uint) (new-market-cap uint))
  (begin
    (asserts! (is-eq tx-sender admin-address) error-admin-only)
    (asserts! (> new-rate u0) error-invalid-price)
    (asserts! (<= new-fee-percentage u30) error-param-invalid)
    (asserts! (> new-user-limit u0) error-max-too-small)
    (asserts! (>= new-market-cap (var-get marketplace-hour-volume)) error-limit-violation)

    ;; Update platform configuration
    (var-set base-hourly-rate new-rate)
    (var-set platform-fee-percentage new-fee-percentage)
    (var-set user-expertise-limit new-user-limit)
    (var-set marketplace-hour-cap new-market-cap)

    (ok true)))

