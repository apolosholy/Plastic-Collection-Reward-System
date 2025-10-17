(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-collector-exists (err u103))
(define-constant err-collector-not-found (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-order-not-found (err u106))
(define-constant err-invalid-price (err u107))
(define-constant err-order-expired (err u108))
(define-constant err-cannot-fill-own-order (err u109))
(define-constant err-milestone-already-claimed (err u110))

(define-constant milestone-bronze u100)
(define-constant milestone-silver u500)
(define-constant milestone-gold u1000)
(define-constant reward-bronze u100)
(define-constant reward-silver u500)
(define-constant reward-gold u1500)

(define-fungible-token plastic-token)

(define-map collectors
    principal
    {
        total-collected: uint,
        reputation-score: uint,
        last-collection: uint,
        verified: bool,
        milestone-bronze-claimed: bool,
        milestone-silver-claimed: bool,
        milestone-gold-claimed: bool,
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

(define-map marketplace-orders
    uint
    {
        seller: principal,
        token-amount: uint,
        stx-price: uint,
        expiry-height: uint,
        active: bool,
    }
)

(define-data-var record-nonce uint u0)
(define-data-var tokens-per-kg uint u10)
(define-data-var min-collection uint u1)
(define-data-var reputation-decay-blocks uint u144)
(define-data-var order-nonce uint u0)
(define-data-var leaderboard-size uint u10)

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
            milestone-bronze-claimed: false,
            milestone-silver-claimed: false,
            milestone-gold-claimed: false,
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
                reputation-score: (calculate-reputation tx-sender current-height),
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

(define-private (calculate-reputation
        (collector principal)
        (current-height uint)
    )
    (let (
            (collector-data (unwrap-panic (map-get? collectors collector)))
            (last-collection-height (get last-collection collector-data))
            (total-collected (get total-collected collector-data))
            (blocks-since-last (if (> current-height last-collection-height)
                (- current-height last-collection-height)
                u0
            ))
            (decay-factor (if (> blocks-since-last (var-get reputation-decay-blocks))
                (/ (var-get reputation-decay-blocks) blocks-since-last)
                u1
            ))
        )
        (/ (* total-collected decay-factor) u10)
    )
)

(define-read-only (get-reputation (collector principal))
    (match (map-get? collectors collector)
        collector-data (some (calculate-reputation collector stacks-block-height))
        none
    )
)

(define-public (create-sell-order
        (token-amount uint)
        (stx-price uint)
        (expiry-blocks uint)
    )
    (let (
            (order-id (var-get order-nonce))
            (expiry-height (+ stacks-block-height expiry-blocks))
        )
        (asserts! (> token-amount u0) err-invalid-amount)
        (asserts! (> stx-price u0) err-invalid-price)
        (asserts! (>= (ft-get-balance plastic-token tx-sender) token-amount)
            err-insufficient-balance
        )
        (try! (ft-transfer? plastic-token token-amount tx-sender
            (as-contract tx-sender)
        ))
        (map-set marketplace-orders order-id {
            seller: tx-sender,
            token-amount: token-amount,
            stx-price: stx-price,
            expiry-height: expiry-height,
            active: true,
        })
        (var-set order-nonce (+ order-id u1))
        (ok order-id)
    )
)

(define-public (buy-tokens (order-id uint))
    (let (
            (order-data (unwrap! (map-get? marketplace-orders order-id) err-order-not-found))
            (seller (get seller order-data))
            (token-amount (get token-amount order-data))
            (stx-price (get stx-price order-data))
            (expiry-height (get expiry-height order-data))
            (active (get active order-data))
        )
        (asserts! active err-order-not-found)
        (asserts! (< stacks-block-height expiry-height) err-order-expired)
        (asserts! (not (is-eq tx-sender seller)) err-cannot-fill-own-order)
        (try! (stx-transfer? stx-price tx-sender seller))
        (try! (as-contract (ft-transfer? plastic-token token-amount (as-contract tx-sender)
            tx-sender
        )))
        (map-set marketplace-orders order-id (merge order-data { active: false }))
        (ok true)
    )
)

(define-public (cancel-order (order-id uint))
    (let (
            (order-data (unwrap! (map-get? marketplace-orders order-id) err-order-not-found))
            (seller (get seller order-data))
            (token-amount (get token-amount order-data))
            (active (get active order-data))
        )
        (asserts! (is-eq tx-sender seller) err-not-authorized)
        (asserts! active err-order-not-found)
        (try! (as-contract (ft-transfer? plastic-token token-amount (as-contract tx-sender) seller)))
        (map-set marketplace-orders order-id (merge order-data { active: false }))
        (ok true)
    )
)

(define-read-only (get-order (order-id uint))
    (map-get? marketplace-orders order-id)
)

(define-read-only (get-leaderboard-by-collection (limit uint))
    (let (
            (max-limit (if (> limit (var-get leaderboard-size))
                (var-get leaderboard-size)
                limit
            ))
            (all-collectors (list))
        )
        (get-top-collectors-by-collection max-limit)
    )
)

(define-read-only (get-leaderboard-by-reputation (limit uint))
    (let ((max-limit (if (> limit (var-get leaderboard-size))
            (var-get leaderboard-size)
            limit
        )))
        (get-top-collectors-by-reputation max-limit)
    )
)

(define-read-only (get-collector-rank-by-collection (collector principal))
    (let (
            (collector-data (unwrap! (map-get? collectors collector) none))
            (total-collected (get total-collected collector-data))
        )
        (some (calculate-collection-rank collector total-collected))
    )
)

(define-read-only (get-collector-rank-by-reputation (collector principal))
    (match (get-reputation collector)
        reputation-score (some (calculate-reputation-rank collector reputation-score))
        none
    )
)

(define-private (get-top-collectors-by-collection (limit uint))
    (ok limit)
)

(define-private (get-top-collectors-by-reputation (limit uint))
    (ok limit)
)

(define-private (calculate-collection-rank
        (collector principal)
        (amount uint)
    )
    u1
)

(define-private (calculate-reputation-rank
        (collector principal)
        (reputation uint)
    )
    u1
)

(define-public (claim-milestone-bronze)
    (let (
            (collector-data (unwrap! (map-get? collectors tx-sender) err-collector-not-found))
            (total-collected (get total-collected collector-data))
            (already-claimed (get milestone-bronze-claimed collector-data))
        )
        (asserts! (>= total-collected milestone-bronze) err-invalid-amount)
        (asserts! (not already-claimed) err-milestone-already-claimed)
        (try! (ft-mint? plastic-token reward-bronze tx-sender))
        (ok (map-set collectors tx-sender
            (merge collector-data { milestone-bronze-claimed: true })
        ))
    )
)

(define-public (claim-milestone-silver)
    (let (
            (collector-data (unwrap! (map-get? collectors tx-sender) err-collector-not-found))
            (total-collected (get total-collected collector-data))
            (already-claimed (get milestone-silver-claimed collector-data))
        )
        (asserts! (>= total-collected milestone-silver) err-invalid-amount)
        (asserts! (not already-claimed) err-milestone-already-claimed)
        (try! (ft-mint? plastic-token reward-silver tx-sender))
        (ok (map-set collectors tx-sender
            (merge collector-data { milestone-silver-claimed: true })
        ))
    )
)

(define-public (claim-milestone-gold)
    (let (
            (collector-data (unwrap! (map-get? collectors tx-sender) err-collector-not-found))
            (total-collected (get total-collected collector-data))
            (already-claimed (get milestone-gold-claimed collector-data))
        )
        (asserts! (>= total-collected milestone-gold) err-invalid-amount)
        (asserts! (not already-claimed) err-milestone-already-claimed)
        (try! (ft-mint? plastic-token reward-gold tx-sender))
        (ok (map-set collectors tx-sender
            (merge collector-data { milestone-gold-claimed: true })
        ))
    )
)

(define-read-only (get-milestone-status (collector principal))
    (match (map-get? collectors collector)
        collector-data (some {
            total-collected: (get total-collected collector-data),
            bronze-eligible: (>= (get total-collected collector-data) milestone-bronze),
            bronze-claimed: (get milestone-bronze-claimed collector-data),
            silver-eligible: (>= (get total-collected collector-data) milestone-silver),
            silver-claimed: (get milestone-silver-claimed collector-data),
            gold-eligible: (>= (get total-collected collector-data) milestone-gold),
            gold-claimed: (get milestone-gold-claimed collector-data),
        })
        none
    )
)

(define-read-only (get-next-milestone (collector principal))
    (match (map-get? collectors collector)
        collector-data (let (
                (total-collected (get total-collected collector-data))
                (bronze-claimed (get milestone-bronze-claimed collector-data))
                (silver-claimed (get milestone-silver-claimed collector-data))
                (gold-claimed (get milestone-gold-claimed collector-data))
            )
            (if (and (< total-collected milestone-bronze) (not bronze-claimed))
                (some {
                    milestone: milestone-bronze,
                    reward: reward-bronze,
                    remaining: (- milestone-bronze total-collected),
                })
                (if (and (< total-collected milestone-silver) (not silver-claimed))
                    (some {
                        milestone: milestone-silver,
                        reward: reward-silver,
                        remaining: (- milestone-silver total-collected),
                    })
                    (if (and (< total-collected milestone-gold) (not gold-claimed))
                        (some {
                            milestone: milestone-gold,
                            reward: reward-gold,
                            remaining: (- milestone-gold total-collected),
                        })
                        none
                    )
                )
            )
        )
        none
    )
)
