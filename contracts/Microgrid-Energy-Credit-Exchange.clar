(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-order-not-found (err u105))
(define-constant err-invalid-price (err u106))
(define-constant err-order-exists (err u107))
(define-constant err-loan-not-found (err u108))
(define-constant err-loan-active (err u109))

(define-data-var total-energy-credits uint u0)
(define-data-var next-order-id uint u1)
(define-data-var next-loan-id uint u1)

(define-map user-credits 
  principal 
  {energy-balance: uint, contribution: uint, consumption: uint})

(define-map energy-prices
  uint 
  uint)

(define-map market-orders
  uint
  {seller: principal, amount: uint, price-per-credit: uint, is-active: bool})

(define-map user-sell-orders
  principal
  uint)

(define-map active-loans
  uint
  {lender: principal, borrower: principal, amount: uint, interest-rate: uint, duration: uint, created-at: uint})

(define-public (initialize-user)
  (let ((user-data {energy-balance: u0, contribution: u0, consumption: u0}))
    (ok (map-set user-credits tx-sender user-data))))

(define-public (contribute-energy (amount uint))
  (let ((user (unwrap! (map-get? user-credits tx-sender) (err u104)))
        (new-contribution (+ (get contribution user) amount))
        (new-balance (+ (get energy-balance user) amount)))
    (map-set user-credits tx-sender
      (merge user {
        contribution: new-contribution,
        energy-balance: new-balance
      }))
    (var-set total-energy-credits (+ (var-get total-energy-credits) amount))
    (match (map-get? user-referrer tx-sender)
      referrer
      (let ((referrer-data (unwrap! (map-get? user-credits referrer) err-not-found))
            (bonus (/ (* amount referral-bonus-percent) u100)))
        (map-set user-credits referrer
          (merge referrer-data {
            energy-balance: (+ (get energy-balance referrer-data) bonus)
          })))
      true)
    (ok true)))

(define-public (consume-energy (amount uint))
  (let ((user (unwrap! (map-get? user-credits tx-sender) (err u104))))
    (asserts! (>= (get energy-balance user) amount) err-insufficient-balance)
    (map-set user-credits tx-sender 
      (merge user {
        consumption: (+ (get consumption user) amount),
        energy-balance: (- (get energy-balance user) amount)
      }))
    (ok true)))

(define-public (transfer-credits (recipient principal) (amount uint))
  (let ((sender-data (unwrap! (map-get? user-credits tx-sender) err-not-found))
        (recipient-data (unwrap! (map-get? user-credits recipient) err-not-found)))
    (asserts! (>= (get energy-balance sender-data) amount) err-insufficient-balance)
    (map-set user-credits tx-sender 
      (merge sender-data {
        energy-balance: (- (get energy-balance sender-data) amount)
      }))
    (map-set user-credits recipient 
      (merge recipient-data {
        energy-balance: (+ (get energy-balance recipient-data) amount)
      }))
    (ok true)))

(define-public (set-energy-price (timestamp uint) (price uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set energy-prices timestamp price))))

(define-read-only (get-user-balance (user principal))
  (match (map-get? user-credits user)
    user-data (ok (get energy-balance user-data))
    err-not-found))

(define-read-only (get-user-stats (user principal))
  (match (map-get? user-credits user)
    user-data (ok user-data)
    err-not-found))

(define-read-only (get-energy-price (timestamp uint))
  (match (map-get? energy-prices timestamp)
    price (ok price)
    err-not-found))

(define-read-only (get-total-credits)
  (ok (var-get total-energy-credits)))

(define-public (create-sell-order (amount uint) (price-per-credit uint))
  (let ((user-data (unwrap! (map-get? user-credits tx-sender) err-not-found))
        (order-id (var-get next-order-id)))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> price-per-credit u0) err-invalid-price)
    (asserts! (is-none (map-get? user-sell-orders tx-sender)) err-order-exists)
    (asserts! (>= (get energy-balance user-data) amount) err-insufficient-balance)
    (map-set user-credits tx-sender 
      (merge user-data {
        energy-balance: (- (get energy-balance user-data) amount)
      }))
    (map-set market-orders order-id {
      seller: tx-sender,
      amount: amount,
      price-per-credit: price-per-credit,
      is-active: true
    })
    (map-set user-sell-orders tx-sender order-id)
    (var-set next-order-id (+ order-id u1))
    (ok order-id)))

(define-public (buy-from-order (order-id uint) (credits-to-buy uint))
  (let ((order-data (unwrap! (map-get? market-orders order-id) err-order-not-found))
        (buyer-data (unwrap! (map-get? user-credits tx-sender) err-not-found))
        (seller-data (unwrap! (map-get? user-credits (get seller order-data)) err-not-found)))
    (asserts! (get is-active order-data) err-order-not-found)
    (asserts! (> credits-to-buy u0) err-invalid-amount)
    (asserts! (<= credits-to-buy (get amount order-data)) err-invalid-amount)
    (let ((total-cost (* credits-to-buy (get price-per-credit order-data)))
          (remaining-amount (- (get amount order-data) credits-to-buy)))
      (asserts! (>= (get energy-balance buyer-data) total-cost) err-insufficient-balance)
      (map-set user-credits tx-sender 
        (merge buyer-data {
          energy-balance: (+ (- (get energy-balance buyer-data) total-cost) credits-to-buy)
        }))
      (map-set user-credits (get seller order-data)
        (merge seller-data {
          energy-balance: (+ (get energy-balance seller-data) total-cost)
        }))
      (if (is-eq remaining-amount u0)
        (begin
          (map-set market-orders order-id 
            (merge order-data {is-active: false}))
          (map-delete user-sell-orders (get seller order-data)))
        (map-set market-orders order-id 
          (merge order-data {amount: remaining-amount})))
      (ok true))))

(define-public (cancel-sell-order)
  (let ((order-id (unwrap! (map-get? user-sell-orders tx-sender) err-order-not-found))
        (order-data (unwrap! (map-get? market-orders order-id) err-order-not-found))
        (user-data (unwrap! (map-get? user-credits tx-sender) err-not-found)))
    (asserts! (get is-active order-data) err-order-not-found)
    (asserts! (is-eq (get seller order-data) tx-sender) err-owner-only)
    (map-set user-credits tx-sender 
      (merge user-data {
        energy-balance: (+ (get energy-balance user-data) (get amount order-data))
      }))
    (map-set market-orders order-id 
      (merge order-data {is-active: false}))
    (map-delete user-sell-orders tx-sender)
    (ok true)))

(define-read-only (get-order-details (order-id uint))
  (match (map-get? market-orders order-id)
    order-data (ok order-data)
    err-order-not-found))

(define-read-only (get-user-sell-order (user principal))
  (match (map-get? user-sell-orders user)
    order-id (ok order-id)
    err-order-not-found))
(define-constant staking-period u100)
(define-constant reward-rate u10)
(define-map staked-credits principal {amount: uint, staked-at: uint})
(define-public (stake-credits (amount uint))
  (let ((user-data (unwrap! (map-get? user-credits tx-sender) err-not-found)))
    (asserts! (>= (get energy-balance user-data) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (map-set user-credits tx-sender (merge user-data {energy-balance: (- (get energy-balance user-data) amount)}))
    (ok (map-set staked-credits tx-sender {amount: amount, staked-at: stacks-block-height}))))
(define-public (unstake-credits)
  (let ((staked-data (unwrap! (map-get? staked-credits tx-sender) err-not-found))
        (user-data (unwrap! (map-get? user-credits tx-sender) err-not-found))
        (elapsed-blocks (- stacks-block-height (get staked-at staked-data))))
    (asserts! (>= elapsed-blocks staking-period) err-invalid-amount)
    (let ((reward (/ (* (get amount staked-data) reward-rate) u100))
          (total-return (+ (get amount staked-data) reward)))
      (map-set user-credits tx-sender (merge user-data {energy-balance: (+ (get energy-balance user-data) total-return)}))
      (map-delete staked-credits tx-sender)
      (ok true))))

(define-public (create-loan (borrower principal) (amount uint) (interest-rate uint) (duration uint))
  (let ((lender-data (unwrap! (map-get? user-credits tx-sender) err-not-found))
        (borrower-data (unwrap! (map-get? user-credits borrower) err-not-found))
        (loan-id (var-get next-loan-id)))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (get energy-balance lender-data) amount) err-insufficient-balance)
    (map-set user-credits tx-sender
      (merge lender-data {
        energy-balance: (- (get energy-balance lender-data) amount)
      }))
    (map-set user-credits borrower
      (merge borrower-data {
        energy-balance: (+ (get energy-balance borrower-data) amount)
      }))
    (map-set active-loans loan-id {
      lender: tx-sender,
      borrower: borrower,
      amount: amount,
      interest-rate: interest-rate,
      duration: duration,
      created-at: stacks-block-height
    })
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)))

(define-public (repay-loan (loan-id uint))
  (let ((loan-data (unwrap! (map-get? active-loans loan-id) err-loan-not-found))
        (borrower-data (unwrap! (map-get? user-credits tx-sender) err-not-found))
        (lender-data (unwrap! (map-get? user-credits (get lender loan-data)) err-not-found)))
    (asserts! (is-eq tx-sender (get borrower loan-data)) err-owner-only)
    (asserts! (>= (get energy-balance borrower-data) (get amount loan-data)) err-insufficient-balance)
    (let ((interest (/ (* (get amount loan-data) (get interest-rate loan-data)) u100))
          (total-repayment (+ (get amount loan-data) interest)))
      (asserts! (>= (get energy-balance borrower-data) total-repayment) err-insufficient-balance)
      (map-set user-credits tx-sender
        (merge borrower-data {
          energy-balance: (- (get energy-balance borrower-data) total-repayment)
        }))
      (map-set user-credits (get lender loan-data)
        (merge lender-data {
          energy-balance: (+ (get energy-balance lender-data) total-repayment)
        }))
      (map-delete active-loans loan-id)
      (ok true))))

(define-read-only (get-loan-details (loan-id uint))
  (match (map-get? active-loans loan-id)
    loan-data (ok loan-data)
    err-loan-not-found))

(define-constant referral-bonus-percent u10)
(define-map user-referrer principal principal)

(define-public (set-referrer (referrer principal))
  (begin
    (asserts! (not (is-eq tx-sender referrer)) err-invalid-amount)
    (asserts! (is-some (map-get? user-credits referrer)) err-not-found)
    (asserts! (is-none (map-get? user-referrer tx-sender)) err-order-exists)
    (ok (map-set user-referrer tx-sender referrer))))
