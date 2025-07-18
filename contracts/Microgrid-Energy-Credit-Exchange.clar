(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))

(define-data-var total-energy-credits uint u0)

(define-map user-credits 
  principal 
  {energy-balance: uint, contribution: uint, consumption: uint})

(define-map energy-prices
  uint 
  uint)

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