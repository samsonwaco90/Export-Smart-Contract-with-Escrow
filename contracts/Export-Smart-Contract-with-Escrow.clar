(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-INITIALIZED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-WRONG-STATUS (err u103))
(define-constant ERR-EXPIRED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))

(define-data-var contract-owner principal tx-sender)

(define-map trade-deals
    uint
    {
        buyer: principal,
        seller: principal,
        amount: uint,
        deadline: uint,
        status: (string-ascii 20),
        delivery-proof: (optional (string-ascii 256)),
        dispute-reason: (optional (string-ascii 256))
    }
)

(define-map escrow-balances
    uint 
    uint
)

(define-data-var deal-counter uint u0)

(define-public (initialize (owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner owner)
        (ok true)
    )
)

(define-public (create-trade-deal 
    (seller principal)
    (amount uint)
    (deadline uint)
)
    (let
        (
            (deal-id (+ (var-get deal-counter) u1))
        )
        (asserts! (>= deadline stacks-block-height) ERR-EXPIRED)
        (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set trade-deals deal-id {
            buyer: tx-sender,
            seller: seller,
            amount: amount,
            deadline: deadline,
            status: "PENDING",
            delivery-proof: none,
            dispute-reason: none
        })
        
        (map-set escrow-balances deal-id amount)
        (var-set deal-counter deal-id)
        (ok deal-id)
    )
)(define-public (submit-delivery-proof 
    (deal-id uint)
    (proof (string-ascii 256))
)
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get seller deal) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status deal) "PENDING") ERR-WRONG-STATUS)
        
        (map-set trade-deals deal-id (merge deal {
            status: "DELIVERED",
            delivery-proof: (some proof)
        }))
        (ok true)
    )
)

(define-public (release-payment (deal-id uint))
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (escrow-amount (unwrap! (map-get? escrow-balances deal-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get status deal) "DELIVERED") ERR-WRONG-STATUS)
        (asserts! (is-eq (get buyer deal) tx-sender) ERR-NOT-AUTHORIZED)
        
        (try! (as-contract (stx-transfer? escrow-amount tx-sender (get seller deal))))
        (map-delete escrow-balances deal-id)
        
        (map-set trade-deals deal-id (merge deal {
            status: "COMPLETED"
        }))
        (ok true)
    )
)

(define-public (raise-dispute 
    (deal-id uint)
    (reason (string-ascii 256))
)
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get buyer deal) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status deal) "PENDING") ERR-WRONG-STATUS)
        
        (map-set trade-deals deal-id (merge deal {
            status: "DISPUTED",
            dispute-reason: (some reason)
        }))
        (ok true)
    )
)

(define-read-only (get-trade-deal (deal-id uint))
    (ok (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
)

(define-read-only (get-escrow-balance (deal-id uint))
    (ok (unwrap! (map-get? escrow-balances deal-id) ERR-NOT-FOUND))
)
