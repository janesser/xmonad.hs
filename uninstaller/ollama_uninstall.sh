#!/bin/bash

# https://medium.com/@heartonbit/uninstalling-ollama-db78dd9545fa

# Define colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🤖 Ollama Uninstallation Script${NC}"
echo -e "${BLUE}-------------------------${NC}"

# Check if script is run with sudo
if [ "$EUID" -ne 0 ]; then
    echo -e "${RED}❌ This script requires root privileges.${NC}"
    echo -e "${YELLOW}Please run with sudo: sudo $0${NC}"
    exit 1
fi

# Check if Docker is running Ollama
if docker ps -a | grep -q ollama; then
    echo -e "${YELLOW}🐳 Found Docker container for Ollama. Stopping and removing...${NC}"
    if ! docker stop ollama 2>/dev/null; then
        echo -e "${RED}⚠️ Failed to stop Ollama container - it may not exist${NC}"
    elif ! docker rm ollama 2>/dev/null; then
        echo -e "${RED}⚠️ Failed to remove Ollama container - it may not exist${NC}"
    else
        echo -e "${GREEN}✅ Docker cleanup completed.${NC}"
    fi
fi

# Check if Ollama is installed locally
if command -v ollama &> /dev/null; then
    echo -e "${YELLOW}🔍 Found local Ollama installation. Removing...${NC}"
    
    # Stop and disable service
    echo -e "${YELLOW}⏹️  Stopping Ollama service...${NC}"
    systemctl stop ollama
    
    echo -e "${YELLOW}🚫 Disabling Ollama service...${NC}"
    systemctl disable ollama
    
    # Remove service file
    echo -e "${YELLOW}🗑️  Removing service file...${NC}"
    rm -f /etc/systemd/system/ollama.service
    
    # Delete binary
    echo -e "${YELLOW}🗑️  Removing Ollama binary...${NC}"
    rm -f $(which ollama)
    
    # Cleanup remaining files and user/group
    echo -e "${YELLOW}🧹 Cleaning up remaining files and user/group...${NC}"
    rm -rf /usr/share/ollama
    userdel ollama
    groupdel ollama
    
    echo -e "${GREEN}✅ Local Ollama installation removed.${NC}"
else
    echo -e "${RED}❌ No local Ollama installation found.${NC}"
fi

echo -e "${BLUE}-------------------------${NC}"
echo -e "${GREEN}✨ Ollama uninstallation completed!${NC}"
