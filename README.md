# Plastic Collection Reward System
A blockchain-based incentive system for plastic collection and recycling in underserved areas.

## 🎯 Overview

This smart contract implements a reward system that:
- Tokenizes plastic collection efforts
- Verifies and tracks collectors
- Manages collection centers
- Issues rewards in fungible tokens

## ⚙️ Features

- Collector registration and verification
- Collection record management
- Automated token rewards
- Reputation tracking
- Collection center management

## 📝 Contract Functions

### Administrative
- `initialize-collection-center`: Set up new collection centers
- `verify-collector`: Authorize collectors
- `update-tokens-per-kg`: Modify reward rate
- `update-min-collection`: Update minimum collection threshold

### Collector Operations
- `register-collector`: Join the system
- `record-collection`: Log plastic collection
- `transfer-tokens`: Transfer earned tokens
- `get-collector-info`: View collector details

### Read-Only Functions
- `get-collection-center-info`: View center details
- `get-collection-record`: Access collection records
- `get-tokens-per-kg`: Check current reward rate
- `get-collector-balance`: Check token balance

## 🚀 Getting Started

1. Deploy the contract using Clarinet
2. Initialize collection centers
3. Register and verify collectors
4. Start recording collections

## 💡 Usage Example

```clarity
;; Register as a collector
(contract-call? .plastic-collection-reward-system register-collector)

;; Record a collection (in kg)
(contract-call? .plastic-collection-reward-system record-collection u10)
```

## 🔒 Security

- Owner-only administrative functions
- Verification requirements
- Balance checks
- Collection amount validation
```
