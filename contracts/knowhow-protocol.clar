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
