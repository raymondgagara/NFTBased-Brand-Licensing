# Brand Rights Smart Contract

A Clarity smart contract for managing NFT-based brand licensing on the Stacks blockchain. This contract enables brand owners to issue, manage, and monetize usage rights for their logos and brand assets with full transparency.

## Features

- Register brands with metadata
- Issue time-limited licenses with specific usage rights
- Collect royalty payments
- Transfer license ownership
- Revoke licenses when terms are violated
- Track royalty payments and license validity

## Contract Functions

### Brand Management

- `register-brand`: Register a new brand with name, description, and metadata URI
- `get-brand-details`: Get information about a registered brand
- `get-brand-owner`: Get the owner of a specific brand

### License Management

- `issue-license`: Issue a new license for a brand to a specific licensee
- `revoke-license`: Revoke a license (brand owner only)
- `extend-license`: Extend the duration of an existing license
- `transfer-license`: Transfer license ownership to another principal
- `is-license-valid`: Check if a license is currently valid
- `get-license-details`: Get detailed information about a license
- `get-license-owner`: Get the current owner of a license

### Royalty Management

- `pay-royalty`: Pay royalties for using a licensed brand
- `get-royalties-collected`: Get total royalties collected for a brand

## Usage Examples

### Registering a Brand

```clarity
(contract-call? .brand-rights register-brand "Acme Corp" "Global technology company" "ipfs://QmXyZ123...")
```

### Issuing a License

```clarity
;; Parameters: brand-id, licensee, duration (blocks), usage-rights, royalty-percentage, territory
(contract-call? .brand-rights issue-license u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u52560 "Digital marketing only" u5 "North America")
```

### Paying Royalties

```clarity
;; Parameters: license-id, amount (in microSTX)
(contract-call? .brand-rights pay-royalty u1 u1000000)
```

### Checking License Validity

```clarity
(contract-call? .brand-rights is-license-valid u1)
```

## License

This project is licensed under the MIT License.

