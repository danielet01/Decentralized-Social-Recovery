;; Decentralized Social Recovery Smart Contract
;; Allows wallet owners to set up trusted guardians who can help recover access
;; through social consensus when the original key is lost

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-WALLET-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-GUARDIAN (err u102))
(define-constant ERR-NOT-GUARDIAN (err u103))
(define-constant ERR-INSUFFICIENT-GUARDIANS (err u104))
(define-constant ERR-RECOVERY-NOT-INITIATED (err u105))
(define-constant ERR-RECOVERY-ALREADY-EXISTS (err u106))
(define-constant ERR-VOTING-PERIOD-ENDED (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-THRESHOLD-NOT-MET (err u109))
(define-constant ERR-RECOVERY-PERIOD-EXPIRED (err u110))
(define-constant ERR-INVALID-THRESHOLD (err u111))

;; Constants
(define-constant VOTING-PERIOD u144) ;; ~24 hours in blocks (10min blocks)
(define-constant RECOVERY-DELAY u1008) ;; ~7 days delay before recovery
(define-constant MIN-GUARDIANS u3)
(define-constant MAX-GUARDIANS u10)

;; Data structures
(define-map wallets
  { owner: principal }
  {
    guardians: (list 10 principal),
    threshold: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map recovery-requests
  { wallet-owner: principal }
  {
    new-owner: principal,
    initiated-at: uint,
    votes: uint,
    voters: (list 10 principal),
    is-executed: bool,
    voting-ends-at: uint
  }
)

(define-map guardian-votes
  { wallet-owner: principal, guardian: principal }
  { has-voted: bool, vote-block: uint }
)

;; Private functions
(define-private (is-guardian (wallet-owner principal) (guardian principal))
  (match (map-get? wallets { owner: wallet-owner })
    wallet-data
    (is-some (index-of (get guardians wallet-data) guardian))
    false
  )
)

(define-private (count-votes (wallet-owner principal))
  (match (map-get? recovery-requests { wallet-owner: wallet-owner })
    request (get votes request)
    u0
  )
)

(define-private (has-voted (wallet-owner principal) (guardian principal))
  (match (map-get? guardian-votes { wallet-owner: wallet-owner, guardian: guardian })
    vote-data (get has-voted vote-data)
    false
  )
)

(define-private (add-guardian-to-list (guardians (list 10 principal)) (new-guardian principal))
  (unwrap-panic (as-max-len? (append guardians new-guardian) u10))
)

(define-private (remove-guardian-from-list 
  (guardians (list 10 principal)) 
  (guardian-to-remove principal))
  (fold remove-if-match guardians (list))
)

(define-private (remove-if-match (guardian principal) (acc (list 10 principal)))
  (if (is-eq guardian (var-get target-guardian))
    acc
    (unwrap-panic (as-max-len? (append acc guardian) u10))
  )
)

(define-data-var target-guardian principal tx-sender)

;; Public functions

;; Setup wallet with initial guardians
(define-public (setup-wallet 
  (guardians (list 10 principal)) 
  (threshold uint))
  (let (
    (guardian-count (len guardians))
  )
    (asserts! (>= guardian-count MIN-GUARDIANS) ERR-INSUFFICIENT-GUARDIANS)
    (asserts! (<= guardian-count MAX-GUARDIANS) ERR-INSUFFICIENT-GUARDIANS)
    (asserts! (and (>= threshold u2) (<= threshold guardian-count)) ERR-INVALID-THRESHOLD)
    (asserts! (is-none (map-get? wallets { owner: tx-sender })) ERR-RECOVERY-ALREADY-EXISTS)
    
    (map-set wallets
      { owner: tx-sender }
      {
        guardians: guardians,
        threshold: threshold,
        is-active: true,
        created-at: block-height
      }
    )
    (ok true)
  )
)

;; Add a new guardian
(define-public (add-guardian (new-guardian principal))
  (let (
    (wallet-data (unwrap! (map-get? wallets { owner: tx-sender }) ERR-WALLET-NOT-FOUND))
    (current-guardians (get guardians wallet-data))
  )
    (asserts! (< (len current-guardians) MAX-GUARDIANS) ERR-INSUFFICIENT-GUARDIANS)
    (asserts! (is-none (index-of current-guardians new-guardian)) ERR-ALREADY-GUARDIAN)
    
    (map-set wallets
      { owner: tx-sender }
      (merge wallet-data {
        guardians: (add-guardian-to-list current-guardians new-guardian)
      })
    )
    (ok true)
  )
)

;; Remove a guardian
(define-public (remove-guardian (guardian principal))
  (let (
    (wallet-data (unwrap! (map-get? wallets { owner: tx-sender }) ERR-WALLET-NOT-FOUND))
    (current-guardians (get guardians wallet-data))
  )
    (asserts! (is-some (index-of current-guardians guardian)) ERR-NOT-GUARDIAN)
    (asserts! (> (len current-guardians) MIN-GUARDIANS) ERR-INSUFFICIENT-GUARDIANS)
    
    (var-set target-guardian guardian)
    (map-set wallets
      { owner: tx-sender }
      (merge wallet-data {
        guardians: (remove-guardian-from-list current-guardians guardian)
      })
    )
    (ok true)
  )
)

;; Update threshold
(define-public (update-threshold (new-threshold uint))
  (let (
    (wallet-data (unwrap! (map-get? wallets { owner: tx-sender }) ERR-WALLET-NOT-FOUND))
    (guardian-count (len (get guardians wallet-data)))
  )
    (asserts! (and (>= new-threshold u2) (<= new-threshold guardian-count)) ERR-INVALID-THRESHOLD)
    
    (map-set wallets
      { owner: tx-sender }
      (merge wallet-data { threshold: new-threshold })
    )
    (ok true)
  )
)

;; Initiate recovery process
(define-public (initiate-recovery (wallet-owner principal) (new-owner principal))
  (let (
    (wallet-data (unwrap! (map-get? wallets { owner: wallet-owner }) ERR-WALLET-NOT-FOUND))
  )
    (asserts! (is-guardian wallet-owner tx-sender) ERR-NOT-GUARDIAN)
    (asserts! (is-none (map-get? recovery-requests { wallet-owner: wallet-owner })) 
              ERR-RECOVERY-ALREADY-EXISTS)
    
    (map-set recovery-requests
      { wallet-owner: wallet-owner }
      {
        new-owner: new-owner,
        initiated-at: block-height,
        votes: u1,
        voters: (list tx-sender),
        is-executed: false,
        voting-ends-at: (+ block-height VOTING-PERIOD)
      }
    )
    
    (map-set guardian-votes
      { wallet-owner: wallet-owner, guardian: tx-sender }
      { has-voted: true, vote-block: block-height }
    )
    
    (ok true)
  )
)

;; Vote for recovery
(define-public (vote-for-recovery (wallet-owner principal))
  (let (
    (wallet-data (unwrap! (map-get? wallets { owner: wallet-owner }) ERR-WALLET-NOT-FOUND))
    (recovery-data (unwrap! (map-get? recovery-requests { wallet-owner: wallet-owner }) 
                            ERR-RECOVERY-NOT-INITIATED))
  )
    (asserts! (is-guardian wallet-owner tx-sender) ERR-NOT-GUARDIAN)
    (asserts! (not (has-voted wallet-owner tx-sender)) ERR-ALREADY-VOTED)
    (asserts! (<= block-height (get voting-ends-at recovery-data)) ERR-VOTING-PERIOD-ENDED)
    
    (let (
      (new-votes (+ (get votes recovery-data) u1))
      (new-voters (unwrap-panic (as-max-len? (append (get voters recovery-data) tx-sender) u10)))
    )
      (map-set recovery-requests
        { wallet-owner: wallet-owner }
        (merge recovery-data {
          votes: new-votes,
          voters: new-voters
        })
      )
      
      (map-set guardian-votes
        { wallet-owner: wallet-owner, guardian: tx-sender }
        { has-voted: true, vote-block: block-height }
      )
      
      (ok true)
    )
  )
)

;; Execute recovery after delay period
(define-public (execute-recovery (wallet-owner principal))
  (let (
    (wallet-data (unwrap! (map-get? wallets { owner: wallet-owner }) ERR-WALLET-NOT-FOUND))
    (recovery-data (unwrap! (map-get? recovery-requests { wallet-owner: wallet-owner }) 
                            ERR-RECOVERY-NOT-INITIATED))
    (threshold (get threshold wallet-data))
  )
    (asserts! (>= (get votes recovery-data) threshold) ERR-THRESHOLD-NOT-MET)
    (asserts! (not (get is-executed recovery-data)) ERR-RECOVERY-ALREADY-EXISTS)
    (asserts! (>= block-height (+ (get initiated-at recovery-data) RECOVERY-DELAY)) 
              ERR-RECOVERY-PERIOD-EXPIRED)
    
    ;; Update wallet owner
    (map-set wallets
      { owner: (get new-owner recovery-data) }
      (merge wallet-data { created-at: block-height })
    )
    
    ;; Remove old wallet entry
    (map-delete wallets { owner: wallet-owner })
    
    ;; Mark recovery as executed
    (map-set recovery-requests
      { wallet-owner: wallet-owner }
      (merge recovery-data { is-executed: true })
    )
    
    (ok true)
  )
)

;; Cancel recovery (only wallet owner can cancel)
(define-public (cancel-recovery)
  (let (
    (recovery-data (unwrap! (map-get? recovery-requests { wallet-owner: tx-sender }) 
                            ERR-RECOVERY-NOT-INITIATED))
  )
    (asserts! (not (get is-executed recovery-data)) ERR-RECOVERY-ALREADY-EXISTS)
    
    (map-delete recovery-requests { wallet-owner: tx-sender })
    (ok true)
  )
)

;; Read-only functions

;; Get wallet info
(define-read-only (get-wallet-info (owner principal))
  (map-get? wallets { owner: owner })
)

;; Get recovery request info
(define-read-only (get-recovery-info (wallet-owner principal))
  (map-get? recovery-requests { wallet-owner: wallet-owner })
)

;; Check if address is guardian
(define-read-only (is-wallet-guardian (wallet-owner principal) (guardian principal))
  (is-guardian wallet-owner guardian)
)

;; Get vote status
(define-read-only (get-vote-status (wallet-owner principal) (guardian principal))
  (map-get? guardian-votes { wallet-owner: wallet-owner, guardian: guardian })
)