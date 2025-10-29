# Ubuntu Agent Setup Guide

This is an agent-executable version of the Ubuntu setup guide. Each section includes **Check** and **Apply** commands to make the process idempotent - the agent can verify if a step is already completed before executing it.

---

## Prerequisites Check

**Check:**
```bash
which git
```

**Apply:**
```bash
sudo apt-get update
sudo apt-get install -y git curl
```

---

## SSH Key Generation

**Check:**
```bash
ls -la ~/.ssh/id_rsa.pub 2>/dev/null | grep -q id_rsa.pub && echo "SSH key exists" || echo "No SSH key found"
```

**Apply:**
```bash
# Generate SSH keys - see terminal/ssh.md for detailed instructions
ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
# Install ssh server if planning remote access
sudo apt-get install -y openssh-server
sudo systemctl enable ssh
sudo systemctl start ssh
```

---

## Repository Setup

**Check:**
```bash
ls -la ~/combined/.git 2>/dev/null | grep -q .git && echo "Combined repo exists" || echo "No combined repo found"
```

**Apply:**
```bash
mkdir -p ~/combined
cd ~/combined
git clone https://gitlab.com/charlesbrandt/public.git
```

---

## Network Configuration

**Check:**
```bash
# Check if static IP configuration exists in netplan
ls -la /etc/netplan/*.yaml 2>/dev/null | grep -q .yaml && echo "Netplan config exists" || echo "No netplan config found"
# Check if static IPs are configured
ip route show | grep -q "192.168" && echo "Static routes configured" || echo "No static routes"
```

**Apply:**
```bash
# If static IP is required, configure via netplan
# See network.md for detailed configuration options
# Example configuration in /etc/netplan/02-static-ip.yaml:
# network:
#   ethernets:
#     eno1:
#       dhcp4: false
#       addresses: [192.168.1.234/24]
#       routes:
#         - to: default
#           via: 192.168.1.1
#       nameservers:
#         addresses: [8.8.8.8, 4.2.2.2]
# sudo netplan apply
```

---

## Editor Installation - Micro

**Check:**
```bash
which micro && echo "Micro editor installed" || echo "Micro not found"
```

**Apply:**
```bash
sudo apt-get install -y curl
cd ~/Downloads
curl https://getmic.ro | sudo bash
sudo mv micro /usr/bin
cd ~/.config
ln -sf ~/combined/public/system/editors/micro/micro.config micro
# For Wayland clipboard support
sudo apt-get install -y xclip
```

---

## Hosts File Configuration

**Check:**
```bash
# Check if hosts file has been customized (contains more than default entries)
wc -l /etc/hosts | awk '{print $1}' | xargs -I {} bash -c 'if [ {} -gt 2 ]; then echo "Hosts file customized"; else echo "Default hosts file"; fi'
```

**Apply:**
```bash
# Edit /etc/hosts with your local network configuration
sudo nano /etc/hosts
# Add entries like:
# 192.168.1.100 server1
# 192.168.1.101 server2
```

---

## Gnome 3 Settings

**Manual Step:**
```
Apply Gnome 3 settings for trackpad, screen brightness, etc.
See window-managers/gnome3.md for detailed instructions.
```

---

## Desktop Background

**Manual Step:**
```
1. Open file explorer
2. Navigate to desired image
3. Right-click on image and choose "Set as Desktop Background"
```

---

## Terminal Configuration

**Check:**
```bash
# Check if custom .bashrc exists and differs from repo version
diff ~/.bashrc ~/combined/public/system/linux/.bashrc 2>/dev/null && echo "bashrc differs from repo" || echo "bashrc matches repo or doesn't exist"
```

**Apply:**
```bash
cp ~/combined/public/system/linux/.bashrc ~/.bashrc
source ~/.bashrc
```

---

## Browser Setup

**Check:**
```bash
which firefox && echo "Firefox installed" || echo "Firefox not found"
```

**Apply:**
```bash
# Browser configuration is largely manual
# See browsers.md for detailed setup instructions:
# - Profile management
# - Extension installation (uBlock Origin, Copy Selection as Markdown, etc.)
# - Settings configuration
sudo apt-get install -y firefox
```

---

## Password Manager Installation

**Check:**
```bash
which keepassxc && echo "KeePassXC installed" || echo "KeePassXC not found"
```

**Apply:**
```bash
sudo apt-get install -y keepassxc
# See password-manager.md for detailed setup:
# - Database creation and configuration
# - Sync setup with git
# - Mobile app setup (KeePassDX for Android)
```

---

## Search Utilities

**Check:**
```bash
# Check if search utilities are referenced/configured
ls -la ~/combined/public/system/search.md && echo "Search documentation available" || echo "No search docs found"
```

**Apply:**
```bash
# See search.md for search utility setup and configuration
# This is primarily documentation and manual configuration
```

---

## Docker Installation

**Check:**
```bash
which docker && echo "Docker installed" || echo "Docker not found"
```

**Apply:**
```bash
# Install Docker and Docker Compose
# See virtualization/docker.md for detailed instructions
sudo apt-get install -y docker.io docker-compose
sudo systemctl enable docker
sudo systemctl start docker
sudo usermod -aG docker $USER
```

---

## Additional Applications

**Manual Step:**
```
Install additional applications as needed.
See applications.md for recommendations and installation guides.
```

---

## System Monitor

**Manual Step:**
```
1. Launch System Monitor application
2. Configure to show all processes
3. Set up monitoring preferences
```

---

## Final Steps

**Check:**
```bash
# Verify system readiness
echo "System setup verification complete"
```

**Apply:**
```bash
# Restart services if needed
# sudo systemctl restart ssh
# newgrp docker
echo "Ubuntu setup process completed!"
```

---

## Usage Instructions for Agent

1. Run each section's **Check** command first
2. Only execute **Apply** if the check indicates it's needed
3. For **Manual Step** sections, pause and prompt the user
4. Mark each section as completed before proceeding
5. Run the entire process multiple times safely (idempotent)

This setup process can be run repeatedly without causing issues, making it perfect for system maintenance and new machine provisioning.
