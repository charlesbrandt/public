# SOPS + Age

Secrets management for config files. SOPS encrypts individual files (JSON, YAML, ENV, etc.) so they can be safely committed to git. Age handles the actual encryption.

## Why

- API keys, tokens, passwords scattered across machines → hard to track
- Plain-text `.env` files in git → security risk
- SOPS encrypts files at rest; git-safe; edit in-place with automatic re-encrypt

## Concepts

- **SOPS** (Secrets OPerationS) — encrypts/decrypts config files, supports multiple backends (age, AWS KMS, GCP, etc.)
- **age** — modern, simple encryption tool. Single keypair, no complex key management
- **Age keypair** — private key decrypts, public key encrypts. Keep private key secure, share public key freely

## Install

### age

```bash
# From GitHub release
curl -sLO https://github.com/FiloSottile/age/releases/download/v1.2.1/age-v1.2.1-linux-amd64.tar.gz
tar xzf age-v1.2.1-linux-amd64.tar.gz
mv age/age age/age-keygen ~/.local/bin/
```

### SOPS

```bash
# From GitHub release (check for latest version)
curl -sL "https://github.com/getsops/sops/releases/download/v3.12.2/sops-v3.12.2.linux.amd64" \
  -o ~/.local/bin/sops
chmod +x ~/.local/bin/sops
```

Verify:
```bash
age --version
sops --version
```

## Setup

### 1. Generate age keypair

```bash
mkdir -p ~/.config/homelab/.age
age-keygen -o ~/.config/homelab/.age/keys.txt
```

Output includes public key:
```
Public key: age1depcaamxck394nf3cwc5tlrl8yv2fhfeut05dwkun4n5ye5sx4zsl6dqtt
```

**Backup the private key** — copy `.age/keys.txt` to a secure offline location.

### 2. Create SOPS config

Create `.sops.yaml` in your secrets directory:

```yaml
creation_rules:
  - path_regex: \.env$
    age: age1depcaamxck394nf3cwc5tlrl8yv2fhfeut05dwkun4n5ye5sx4zsl6dqtt
```

Replace with your public key. Add multiple rules for different file patterns.

### 3. Add to .gitignore

```
# Never commit private key
.age/keys.txt
```

## Usage

### Encrypt a file

```bash
SOPS_AGE_KEY_FILE=.age/keys.txt sops --encrypt --in-place secrets.env
```

### Decrypt a file (view)

```bash
SOPS_AGE_KEY_FILE=.age/keys.txt sops --decrypt secrets.env
```

### Edit in-place

```bash
SOPS_AGE_KEY_FILE=.age/keys.txt sops --decrypt --in-place secrets.env
# Edit the file, save — sops re-encrypts automatically
```

### Shell alias

Add to `~/.bashrc`:

```bash
export SOPS_AGE_KEY_FILE=~/.config/homelab/.age/keys.txt
```

Then just use `sops --decrypt` or `sops --encrypt` without the env var.

## Multi-Machine Setup

### Simple (one key, copy to all machines)

```bash
# Copy key to another machine
scp ~/.config/homelab/.age/keys.txt otherhost:~/.config/homelab/.age/keys.txt
```

### Better (one key per machine)

Generate a keypair on each machine, add all public keys to `.sops.yaml`:

```yaml
creation_rules:
  - path_regex: \.env$
    age: |
      age1depcaamxck394nf3cwc5tlrl8yv2fhfeut05dwkun4n5ye5sx4zsl6dqtt
      age1abc123def456...
      age1xyz789...
```

If a machine is compromised, remove its public key and re-encrypt.

## File Structure Example

```
~/.config/homelab/
├── .age/
│   └── keys.txt        # Private key (gitignored)
├── .gitignore
├── .sops.yaml          # Encryption rules
├── README.md
├── homeassistant.env   # Encrypted — safe to commit
├── camera-office.env   # Encrypted
└── gitea.env           # Encrypted
```

## Troubleshooting

### "MAC mismatch" error

File was modified without sops. Re-encrypt from the original plaintext.

### "could not decrypt data key"

Wrong age key or corrupted file. Check `SOPS_AGE_KEY_FILE` points to correct key.

### File not re-encrypting after edit

Use `--in-place` flag: `sops --decrypt --in-place file.env`

## References

- SOPS: https://github.com/getsops/sops
- Age: https://github.com/FiloSottile/age
- SOPS + Age guide: https://dev.to/cooljeet/sops-and-age-for-secrets-management-in-git-1f9k
