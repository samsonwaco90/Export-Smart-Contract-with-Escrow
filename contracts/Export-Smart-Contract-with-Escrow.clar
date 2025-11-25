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
)

(define-public (submit-delivery-proof 
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
        
        (increment-user-deals (get buyer deal))
        (increment-user-deals (get seller deal))
        
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

(define-constant ERR-NOT-ARBITRATOR (err u106))
(define-constant ERR-ALREADY-VOTED (err u107))
(define-constant ERR-VOTING-ENDED (err u108))
(define-constant ARBITRATION-FEE u1000)
(define-constant MIN-ARBITRATORS u3)

(define-map arbitrators principal bool)

(define-map dispute-votes
    uint
    {
        votes-for-buyer: uint,
        votes-for-seller: uint,
        total-votes: uint,
        voting-deadline: uint,
        resolved: bool
    }
)

(define-map arbitrator-votes
    {deal-id: uint, arbitrator: principal}
    (string-ascii 10)
)

(define-public (register-arbitrator)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set arbitrators tx-sender true)
        (ok true)
    )
)

(define-public (vote-on-dispute 
    (deal-id uint)
    (vote-for (string-ascii 10))
)
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (dispute-info (unwrap! (map-get? dispute-votes deal-id) ERR-NOT-FOUND))
            (vote-key {deal-id: deal-id, arbitrator: tx-sender})
        )
        (asserts! (default-to false (map-get? arbitrators tx-sender)) ERR-NOT-ARBITRATOR)
        (asserts! (is-eq (get status deal) "DISPUTED") ERR-WRONG-STATUS)
        (asserts! (< stacks-block-height (get voting-deadline dispute-info)) ERR-EXPIRED)
        (asserts! (is-none (map-get? arbitrator-votes vote-key)) ERR-ALREADY-VOTED)
        (asserts! (not (get resolved dispute-info)) ERR-VOTING-ENDED)
        
        (map-set arbitrator-votes vote-key vote-for)
        
        (let
            (
                (new-votes-buyer (if (is-eq vote-for "BUYER") 
                    (+ (get votes-for-buyer dispute-info) u1)
                    (get votes-for-buyer dispute-info)))
                (new-votes-seller (if (is-eq vote-for "SELLER")
                    (+ (get votes-for-seller dispute-info) u1)
                    (get votes-for-seller dispute-info)))
                (new-total (+ (get total-votes dispute-info) u1))
            )
            (map-set dispute-votes deal-id (merge dispute-info {
                votes-for-buyer: new-votes-buyer,
                votes-for-seller: new-votes-seller,
                total-votes: new-total
            }))
        )
        (ok true)
    )
)

(define-public (resolve-dispute (deal-id uint))
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (dispute-info (unwrap! (map-get? dispute-votes deal-id) ERR-NOT-FOUND))
            (escrow-amount (unwrap! (map-get? escrow-balances deal-id) ERR-NOT-FOUND))
        )
        (asserts! (>= stacks-block-height (get voting-deadline dispute-info)) ERR-WRONG-STATUS)
        (asserts! (not (get resolved dispute-info)) ERR-VOTING-ENDED)
        (asserts! (>= (get total-votes dispute-info) MIN-ARBITRATORS) ERR-WRONG-STATUS)
        
        (let
            (
                (winner (if (> (get votes-for-buyer dispute-info) (get votes-for-seller dispute-info))
                    "BUYER" "SELLER"))
                (recipient (if (is-eq winner "BUYER")
                    (get buyer deal)
                    (get seller deal)))
            )
            (try! (as-contract (stx-transfer? escrow-amount tx-sender recipient)))
            (map-delete escrow-balances deal-id)
            
            (increment-user-deals (get buyer deal))
            (increment-user-deals (get seller deal))
            
            (map-set trade-deals deal-id (merge deal {
                status: "RESOLVED"
            }))
            
            (map-set dispute-votes deal-id (merge dispute-info {
                resolved: true
            }))
        )
        (ok true)
    )
)

(define-public (raise-dispute-updated
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
        
        (map-set dispute-votes deal-id {
            votes-for-buyer: u0,
            votes-for-seller: u0,
            total-votes: u0,
            voting-deadline: (+ stacks-block-height u144),
            resolved: false
        })
        (ok true)
    )
)

(define-read-only (get-dispute-votes (deal-id uint))
    (ok (unwrap! (map-get? dispute-votes deal-id) ERR-NOT-FOUND))
)

(define-constant ERR-MILESTONE-NOT-FOUND (err u109))
(define-constant ERR-MILESTONE-COMPLETED (err u110))
(define-constant MAX-MILESTONES u10)

(define-map milestone-deals
    uint
    {
        buyer: principal,
        seller: principal,
        total-amount: uint,
        deadline: uint,
        status: (string-ascii 20),
        completed-milestones: uint,
        total-milestones: uint
    }
)

(define-map deal-milestones
    {deal-id: uint, milestone-id: uint}
    {
        description: (string-ascii 256),
        amount: uint,
        completed: bool,
        proof: (optional (string-ascii 256)),
        completion-date: (optional uint)
    }
)

(define-map milestone-escrow
    uint
    uint
)

(define-data-var milestone-deal-counter uint u0)

(define-private (create-milestone-entry
    (deal-id uint)
    (milestone-id uint)
    (description (string-ascii 256))
    (amount uint)
)
    (begin
        (map-set deal-milestones 
            {deal-id: deal-id, milestone-id: milestone-id}
            {
                description: description,
                amount: amount,
                completed: false,
                proof: none,
                completion-date: none
            }
        )
        true
    )
)

(define-private (process-milestone-entries
    (deal-id uint)
    (descriptions (list 10 (string-ascii 256)))
    (amounts (list 10 uint))
)
    (let 
        (
            (descriptions-with-amounts (zip descriptions amounts))
        )
        (fold process-single-milestone descriptions-with-amounts {deal-id: deal-id, index: u0})
        (ok true)
    )
)

(define-private (process-single-milestone 
    (milestone-data {description: (string-ascii 256), amount: uint})
    (state {deal-id: uint, index: uint})
)
    (let 
        (
            (new-index (+ (get index state) u1))
        )
        (create-milestone-entry 
            (get deal-id state)
            new-index
            (get description milestone-data)
            (get amount milestone-data)
        )
        {deal-id: (get deal-id state), index: new-index}
    )
)

(define-private (zip (list-a (list 10 (string-ascii 256))) (list-b (list 10 uint)))
    (map combine-two list-a list-b)
)

(define-private (combine-two (a (string-ascii 256)) (b uint))
    {description: a, amount: b}
)

(define-public (create-milestone-deal
    (seller principal)
    (total-amount uint)
    (deadline uint)
    (milestone-descriptions (list 10 (string-ascii 256)))
    (milestone-amounts (list 10 uint))
)
    (let
        (
            (deal-id (+ (var-get milestone-deal-counter) u1))
            (milestone-count (len milestone-descriptions))
        )
        (asserts! (>= deadline stacks-block-height) ERR-EXPIRED)
        (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR-INSUFFICIENT-FUNDS)
        (asserts! (<= milestone-count MAX-MILESTONES) ERR-WRONG-STATUS)
        (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR-WRONG-STATUS)
        
        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
        
        (map-set milestone-deals deal-id {
            buyer: tx-sender,
            seller: seller,
            total-amount: total-amount,
            deadline: deadline,
            status: "ACTIVE",
            completed-milestones: u0,
            total-milestones: milestone-count
        })
        
        (map-set milestone-escrow deal-id total-amount)
        (var-set milestone-deal-counter deal-id)
        
        (unwrap-panic (process-milestone-entries deal-id milestone-descriptions milestone-amounts))
        (ok deal-id)
    )
)

(define-public (complete-milestone
    (deal-id uint)
    (milestone-id uint)
    (proof (string-ascii 256))
)
    (let
        (
            (deal (unwrap! (map-get? milestone-deals deal-id) ERR-NOT-FOUND))
            (milestone-key {deal-id: deal-id, milestone-id: milestone-id})
            (milestone (unwrap! (map-get? deal-milestones milestone-key) ERR-MILESTONE-NOT-FOUND))
        )
        (asserts! (is-eq (get seller deal) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status deal) "ACTIVE") ERR-WRONG-STATUS)
        (asserts! (not (get completed milestone)) ERR-MILESTONE-COMPLETED)
        
        (map-set deal-milestones milestone-key (merge milestone {
            completed: true,
            proof: (some proof),
            completion-date: (some stacks-block-height)
        }))
        (ok true)
    )
)

(define-public (release-milestone-payment
    (deal-id uint)
    (milestone-id uint)
)
    (let
        (
            (deal (unwrap! (map-get? milestone-deals deal-id) ERR-NOT-FOUND))
            (milestone-key {deal-id: deal-id, milestone-id: milestone-id})
            (milestone (unwrap! (map-get? deal-milestones milestone-key) ERR-MILESTONE-NOT-FOUND))
            (escrow-amount (unwrap! (map-get? milestone-escrow deal-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get buyer deal) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status deal) "ACTIVE") ERR-WRONG-STATUS)
        (asserts! (get completed milestone) ERR-WRONG-STATUS)
        (asserts! (>= escrow-amount (get amount milestone)) ERR-INSUFFICIENT-FUNDS)
        
        (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get seller deal))))
        
        (let
            (
                (new-escrow (- escrow-amount (get amount milestone)))
                (new-completed (+ (get completed-milestones deal) u1))
            )
            (map-set milestone-escrow deal-id new-escrow)
            
            (map-set milestone-deals deal-id (merge deal {
                completed-milestones: new-completed,
                status: (if (is-eq new-completed (get total-milestones deal))
                    "COMPLETED" "ACTIVE")
            }))
            
            (if (is-eq new-completed (get total-milestones deal))
                (begin
                    (increment-user-deals (get buyer deal))
                    (increment-user-deals (get seller deal))
                    true
                )
                true
            )
        )
        (ok true)
    )
)

(define-read-only (get-milestone-deal (deal-id uint))
    (ok (unwrap! (map-get? milestone-deals deal-id) ERR-NOT-FOUND))
)

(define-read-only (get-milestone
    (deal-id uint)
    (milestone-id uint)
)
    (ok (unwrap! (map-get? deal-milestones {deal-id: deal-id, milestone-id: milestone-id}) ERR-MILESTONE-NOT-FOUND))
)

(define-read-only (get-milestone-escrow (deal-id uint))
    (ok (unwrap! (map-get? milestone-escrow deal-id) ERR-NOT-FOUND))
)

(define-constant ERR-ALREADY-RATED (err u111))
(define-constant ERR-CANNOT-RATE-SELF (err u112))
(define-constant ERR-INVALID-RATING (err u113))

(define-map user-ratings
    {rater: principal, rated: principal, deal-id: uint}
    {
        rating: uint,
        comment: (optional (string-ascii 256)),
        timestamp: uint
    }
)

(define-map user-reputation
    principal
    {
        total-rating: uint,
        rating-count: uint,
        deals-completed: uint
    }
)

(define-map deal-ratings
    uint
    {
        buyer-rated: bool,
        seller-rated: bool,
        avg-rating: uint
    }
)

(define-public (rate-user
    (deal-id uint)
    (rated-user principal)
    (rating uint)
    (comment (optional (string-ascii 256)))
)
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (rating-key {rater: tx-sender, rated: rated-user, deal-id: deal-id})
            (current-reputation (default-to {total-rating: u0, rating-count: u0, deals-completed: u0} 
                (map-get? user-reputation rated-user)))
            (deal-rating-info (default-to {buyer-rated: false, seller-rated: false, avg-rating: u0}
                (map-get? deal-ratings deal-id)))
        )
        (asserts! (or (is-eq (get status deal) "COMPLETED") (is-eq (get status deal) "RESOLVED")) ERR-WRONG-STATUS)
        (asserts! (or (is-eq tx-sender (get buyer deal)) (is-eq tx-sender (get seller deal))) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq tx-sender rated-user)) ERR-CANNOT-RATE-SELF)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-none (map-get? user-ratings rating-key)) ERR-ALREADY-RATED)
        
        (map-set user-ratings rating-key {
            rating: rating,
            comment: comment,
            timestamp: stacks-block-height
        })
        
        (let
            (
                (new-total-rating (+ (get total-rating current-reputation) rating))
                (new-rating-count (+ (get rating-count current-reputation) u1))
                (is-buyer-rating (is-eq tx-sender (get buyer deal)))
                (new-buyer-rated (if is-buyer-rating true (get buyer-rated deal-rating-info)))
                (new-seller-rated (if (not is-buyer-rating) true (get seller-rated deal-rating-info)))
            )
            (map-set user-reputation rated-user {
                total-rating: new-total-rating,
                rating-count: new-rating-count,
                deals-completed: (get deals-completed current-reputation)
            })
            
            (map-set deal-ratings deal-id {
                buyer-rated: new-buyer-rated,
                seller-rated: new-seller-rated,
                avg-rating: (get avg-rating deal-rating-info)
            })
        )
        (ok true)
    )
)

(define-private (increment-user-deals (user principal))
    (let
        (
            (current-reputation (default-to {total-rating: u0, rating-count: u0, deals-completed: u0}
                (map-get? user-reputation user)))
        )
        (map-set user-reputation user (merge current-reputation {
            deals-completed: (+ (get deals-completed current-reputation) u1)
        }))
        true
    )
)

(define-read-only (get-user-reputation (user principal))
    (let
        (
            (reputation (default-to {total-rating: u0, rating-count: u0, deals-completed: u0}
                (map-get? user-reputation user)))
            (avg-rating (if (> (get rating-count reputation) u0)
                (/ (get total-rating reputation) (get rating-count reputation))
                u0))
        )
        (ok {
            average-rating: avg-rating,
            total-ratings: (get rating-count reputation),
            deals-completed: (get deals-completed reputation)
        })
    )
)

(define-read-only (get-deal-rating (deal-id uint))
    (ok (default-to {buyer-rated: false, seller-rated: false, avg-rating: u0}
        (map-get? deal-ratings deal-id)))
)

(define-read-only (get-rating-details
    (rater principal)
    (rated principal)
    (deal-id uint)
)
    (ok (map-get? user-ratings {rater: rater, rated: rated, deal-id: deal-id}))
)

(define-constant ERR-EXTENSION-NOT-FOUND (err u114))
(define-constant ERR-EXTENSION-ALREADY-APPROVED (err u115))
(define-constant ERR-EXTENSION-EXPIRED (err u116))
(define-constant MAX-EXTENSION-BLOCKS u1440)

(define-map deadline-extensions
    uint
    {
        requester: principal,
        new-deadline: uint,
        reason: (string-ascii 256),
        buyer-approved: bool,
        seller-approved: bool,
        request-expiry: uint,
        status: (string-ascii 20)
    }
)


(define-public (request-deadline-extension
    (deal-id uint)
    (new-deadline uint)
    (reason (string-ascii 256))
)
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (current-extension (map-get? deadline-extensions deal-id))
        )
        (asserts! (or (is-eq tx-sender (get buyer deal)) (is-eq tx-sender (get seller deal))) ERR-NOT-AUTHORIZED)
        (asserts! (or (is-eq (get status deal) "PENDING") (is-eq (get status deal) "DELIVERED")) ERR-WRONG-STATUS)
        (asserts! (> new-deadline (get deadline deal)) ERR-WRONG-STATUS)
        (asserts! (<= (- new-deadline (get deadline deal)) MAX-EXTENSION-BLOCKS) ERR-WRONG-STATUS)
        (asserts! (is-none current-extension) ERR-ALREADY-INITIALIZED)
        
        (let
            (
                (is-buyer-request (is-eq tx-sender (get buyer deal)))
                (buyer-approved is-buyer-request)
                (seller-approved (not is-buyer-request))
            )
            (map-set deadline-extensions deal-id {
                requester: tx-sender,
                new-deadline: new-deadline,
                reason: reason,
                buyer-approved: buyer-approved,
                seller-approved: seller-approved,
                request-expiry: (+ stacks-block-height u144),
                status: "PENDING"
            })
        )
        (ok true)
    )
)

(define-public (approve-deadline-extension (deal-id uint))
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (extension (unwrap! (map-get? deadline-extensions deal-id) ERR-EXTENSION-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender (get buyer deal)) (is-eq tx-sender (get seller deal))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status extension) "PENDING") ERR-WRONG-STATUS)
        (asserts! (< stacks-block-height (get request-expiry extension)) ERR-EXTENSION-EXPIRED)
        
        (let
            (
                (is-buyer-approval (is-eq tx-sender (get buyer deal)))
                (new-buyer-approved (if is-buyer-approval true (get buyer-approved extension)))
                (new-seller-approved (if (not is-buyer-approval) true (get seller-approved extension)))
                (both-approved (and new-buyer-approved new-seller-approved))
            )
            (asserts! (or (and is-buyer-approval (not (get buyer-approved extension)))
                         (and (not is-buyer-approval) (not (get seller-approved extension)))) ERR-EXTENSION-ALREADY-APPROVED)
            
            (map-set deadline-extensions deal-id (merge extension {
                buyer-approved: new-buyer-approved,
                seller-approved: new-seller-approved,
                status: (if both-approved "APPROVED" "PENDING")
            }))
            
            (if both-approved
                (begin
                    (map-set trade-deals deal-id (merge deal {
                        deadline: (get new-deadline extension)
                    }))
                    (map-delete deadline-extensions deal-id)
                )
                true
            )
        )
        (ok true)
    )
)

(define-public (reject-deadline-extension (deal-id uint))
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (extension (unwrap! (map-get? deadline-extensions deal-id) ERR-EXTENSION-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender (get buyer deal)) (is-eq tx-sender (get seller deal))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status extension) "PENDING") ERR-WRONG-STATUS)
        
        (map-set deadline-extensions deal-id (merge extension {
            status: "REJECTED"
        }))
        (ok true)
    )
)

(define-read-only (get-deadline-extension (deal-id uint))
    (ok (map-get? deadline-extensions deal-id))
)

(define-constant ERR-REFUND-NOT-AVAILABLE (err u117))
(define-constant ERR-ALREADY-REFUNDED (err u118))
(define-constant ERR-MILESTONE-DEADLINE-NOT-REACHED (err u119))
(define-constant ERR-NO-FUNDS-TO-REFUND (err u120))

(define-map refund-claims
    uint
    {
        claimed: bool,
        claim-date: uint,
        refund-amount: uint
    }
)

(define-map milestone-refund-claims
    uint
    {
        claimed: bool,
        claim-date: uint,
        refund-amount: uint,
        completed-count: uint
    }
)

(define-public (claim-refund (deal-id uint))
    (let
        (
            (deal (unwrap! (map-get? trade-deals deal-id) ERR-NOT-FOUND))
            (escrow-amount (unwrap! (map-get? escrow-balances deal-id) ERR-NOT-FOUND))
            (refund-record (map-get? refund-claims deal-id))
        )
        (asserts! (is-eq (get buyer deal) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status deal) "PENDING") ERR-WRONG-STATUS)
        (asserts! (> stacks-block-height (get deadline deal)) ERR-REFUND-NOT-AVAILABLE)
        (asserts! (is-none refund-record) ERR-ALREADY-REFUNDED)
        
        (try! (as-contract (stx-transfer? escrow-amount tx-sender (get buyer deal))))
        (map-delete escrow-balances deal-id)
        
        (map-set refund-claims deal-id {
            claimed: true,
            claim-date: stacks-block-height,
            refund-amount: escrow-amount
        })
        
        (map-set trade-deals deal-id (merge deal {
            status: "REFUNDED"
        }))
        (ok escrow-amount)
    )
)

(define-read-only (is-refund-available (deal-id uint))
    (match (map-get? trade-deals deal-id)
        deal 
            (ok {
                available: (and 
                    (is-eq (get status deal) "PENDING")
                    (> stacks-block-height (get deadline deal))
                    (is-none (map-get? refund-claims deal-id))
                ),
                deadline: (get deadline deal),
                current-block: stacks-block-height
            })
        ERR-NOT-FOUND
    )
)

(define-read-only (get-refund-claim (deal-id uint))
    (ok (map-get? refund-claims deal-id))
)

(define-public (claim-milestone-partial-refund (deal-id uint))
    (let
        (
            (deal (unwrap! (map-get? milestone-deals deal-id) ERR-NOT-FOUND))
            (escrow-amount (unwrap! (map-get? milestone-escrow deal-id) ERR-NOT-FOUND))
            (refund-record (map-get? milestone-refund-claims deal-id))
        )
        (asserts! (is-eq (get buyer deal) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status deal) "ACTIVE") ERR-WRONG-STATUS)
        (asserts! (> stacks-block-height (get deadline deal)) ERR-MILESTONE-DEADLINE-NOT-REACHED)
        (asserts! (is-none refund-record) ERR-ALREADY-REFUNDED)
        (asserts! (> escrow-amount u0) ERR-NO-FUNDS-TO-REFUND)
        
        (try! (as-contract (stx-transfer? escrow-amount tx-sender (get buyer deal))))
        (map-delete milestone-escrow deal-id)
        
        (map-set milestone-refund-claims deal-id {
            claimed: true,
            claim-date: stacks-block-height,
            refund-amount: escrow-amount,
            completed-count: (get completed-milestones deal)
        })
        
        (map-set milestone-deals deal-id (merge deal {
            status: "REFUNDED"
        }))
        (ok escrow-amount)
    )
)

(define-read-only (is-milestone-refund-available (deal-id uint))
    (match (map-get? milestone-deals deal-id)
        deal
            (ok {
                available: (and
                    (is-eq (get status deal) "ACTIVE")
                    (> stacks-block-height (get deadline deal))
                    (is-none (map-get? milestone-refund-claims deal-id))
                ),
                deadline: (get deadline deal),
                current-block: stacks-block-height,
                uncompleted-milestones: (- (get total-milestones deal) (get completed-milestones deal))
            })
        ERR-NOT-FOUND
    )
)

(define-read-only (get-milestone-refund-claim (deal-id uint))
    (ok (map-get? milestone-refund-claims deal-id))
)

(define-read-only (calculate-milestone-refund-amount (deal-id uint))
    (match (map-get? milestone-deals deal-id)
        deal
            (match (map-get? milestone-escrow deal-id)
                escrow-balance
                    (ok {
                        refundable-amount: escrow-balance,
                        total-amount: (get total-amount deal),
                        completed-milestones: (get completed-milestones deal),
                        total-milestones: (get total-milestones deal)
                    })
                ERR-NOT-FOUND
            )
        ERR-NOT-FOUND
    )
)

