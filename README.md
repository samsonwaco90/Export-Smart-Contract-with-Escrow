# 🌍 Export Smart Contract with Escrow

A secure and transparent smart contract system for international trade settlements using the Stacks blockchain.

## 🎯 Features

- 📝 Create trade deals with escrow
- 💰 Automatic payment locking in escrow
- 🚢 Delivery proof submission
- ✅ Payment release mechanism
- ⚖️ Dispute handling system

## 🔧 Contract Functions

### For Buyers
- `create-trade-deal`: Initialize a new trade with escrow payment
- `release-payment`: Release escrowed funds after successful delivery
- `raise-dispute`: Open a dispute for problematic transactions

### For Sellers
- `submit-delivery-proof`: Submit proof of delivery (e.g., bill of lading)

### Read-Only Functions
- `get-trade-deal`: View details of a specific trade
- `get-escrow-balance`: Check escrow balance for a trade

## 🚀 Usage Example

1. Buyer creates trade deal:
```clarity
(contract-call? .export-smart-contract-with-escrow create-trade-deal 
    'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 
    u1000000 
    u1000)
```

2. Seller submits delivery proof:
```clarity
(contract-call? .export-smart-contract-with-escrow submit-delivery-proof 
    u1 
    "BOL123456789")
```

3. Buyer releases payment:
```clarity
(contract-call? .export-smart-contract-with-escrow release-payment u1)
```

## 🔒 Security Features

- Automated escrow system
- Deadline enforcement
- Authorization checks
- Status-based validations

## 📋 Status Flows

PENDING → DELIVERED → COMPLETED
PENDING → DISPUTED
```
