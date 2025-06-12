(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-collector-exists (err u103))
(define-constant err-collector-not-found (err u104))
(define-constant err-insufficient-balance (err u105))

(define-fungible-token plastic-token)

(define-map collectors
    principal
    {
        total-collected: uint,
        reputation-score: uint,
        last-collection: uint,
        verified: bool,
    }
)

(define-map collection-centers
    principal
    {
        name: (string-ascii 50),
        location: (string-ascii 100),
        active: bool,
        total-processed: uint,
    }
)

(define-map collection-records
    uint
    {
        collector: principal,
        center: principal,
        amount: uint,
        timestamp: uint,
        verified: bool,
    }
)

(define-data-var record-nonce uint u0)
(define-data-var tokens-per-kg uint u10)
(define-data-var min-collection uint u1)

(define-public (initialize-collection-center
        (name (string-ascii 50))
        (location (string-ascii 100))
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set collection-centers tx-sender {
            name: name,
            location: location,
            active: true,
            total-processed: u0,
        }))
    )
)

(define-public (register-collector)
    (begin
        (asserts! (is-none (map-get? collectors tx-sender)) err-collector-exists)
        (ok (map-set collectors tx-sender {
            total-collected: u0,
            reputation-score: u0,
            last-collection: u0,
            verified: false,
        }))
    )
)

(define-public (verify-collector (collector principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (map-get? collectors collector))
            err-collector-not-found
        )
        (ok (map-set collectors collector
            (merge (unwrap-panic (map-get? collectors collector)) { verified: true })
        ))
    )
)

(define-public (record-collection (amount uint))
    (let (
            (collector-data (unwrap! (map-get? collectors tx-sender) err-collector-not-found))
            (current-height stacks-block-height)
            (record-id (var-get record-nonce))
        )
        (asserts! (>= amount (var-get min-collection)) err-invalid-amount)
        (asserts! (get verified collector-data) err-not-authorized)
        (try! (ft-mint? plastic-token (* amount (var-get tokens-per-kg)) tx-sender))
        (map-set collectors tx-sender
            (merge collector-data {
                total-collected: (+ (get total-collected collector-data) amount),
                last-collection: current-height,
            })
        )
        (map-set collection-records record-id {
            collector: tx-sender,
            center: contract-owner,
            amount: amount,
            timestamp: current-height,
            verified: true,
        })
        (var-set record-nonce (+ record-id u1))
        (ok true)
    )
)

(define-public (update-tokens-per-kg (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set tokens-per-kg new-rate))
    )
)

(define-public (update-min-collection (new-min uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set min-collection new-min))
    )
)

(define-public (transfer-tokens
        (recipient principal)
        (amount uint)
    )
    (begin
        (asserts! (is-some (map-get? collectors tx-sender))
            err-collector-not-found
        )
        (ft-transfer? plastic-token amount tx-sender recipient)
    )
)

(define-read-only (get-collector-info (collector principal))
    (map-get? collectors collector)
)

(define-read-only (get-collection-center-info (center principal))
    (map-get? collection-centers center)
)

(define-read-only (get-collection-record (record-id uint))
    (map-get? collection-records record-id)
)

(define-read-only (get-tokens-per-kg)
    (ok (var-get tokens-per-kg))
)

(define-read-only (get-collector-balance (collector principal))
    (ft-get-balance plastic-token collector)
)
