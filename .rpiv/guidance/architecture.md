# Architecture Guidance: Root Project

## Root Project: Configuration Management System (CaC)

### Overview
This project functions as a centralized Configuration Management System (CaC). It utilizes `chezmoi` to manage and synchronize the user's environment across various platforms. The primary goal is to enforce a consistent desired state for the local machine by treating configuration as version-controlled code.

### Core Architectural Principles
*   **Configuration as Code (CaC):** All system settings are defined through version-controlled templates, not hardcoded local files.
*   **State Enforcement:** The primary operation is the execution of `chezmoi apply -k`, which renders these templates into the final system state.
*   **Security First:** Sensitive data is encrypted using the `age` standard, ensuring secrets remain out of plaintext in the repository.
*   **Audience:** This guidance is specifically authored for AI assistants/agents performing maintenance, refactoring, and review tasks.

### System Components Map & Agent Workflow

#### 1. Orchestration & Technology Setup (`.chezmoitscripts`)
The `.chezmoitscripts` directory is the intended location for initial setup of new technologies and package management.
*   **Purpose:** To handle complex, tool-specific configuration (e.g., installing a specific language runtime, compiling from a GitHub repository, or configuring a niche tool).
*   **Agent Instruction:** When a new technology or complex dependency is required, the agent must **first** research the external documentation for that technology. The setup logic should be contained within or referenced by scripts here to isolate the configuration.
*   **Current State:** This directory is currently missing/undefined in the codebase, and new logic must be added here following the research phase.

#### 2. Operational Helpers (`dot_local/bin`)
This directory contains the primary collection of convenience scripts and system maintenance tools.
*   **Purpose:** To provide daily operational utilities, system cleanup routines, and environment management scripts.
*   **Structure & Pattern:** Scripts follow a strict, standardized naming convention: `executable_<action>-<tool_name>`.
    *   **Action Groups:** `cleanup-`, `upgradeall-`, etc.
    *   **Tool Group:** `npm`, `podman`, `pipx`, etc.
*   **Agent Instruction:** Agents must prioritize reusing these existing scripts for maintenance. When refactoring, consistency between present and new scripts in this directory must be maintained.

#### 3. Implementation Lifecycle (The Agent Pipeline)
The process for introducing new functionality or refactoring existing logic is as follows:
1.  **Research:** Identify the technology and its configuration needs.
2.  **Setup:** Implement initial technology setup in `.chezmoitscripts`.
3.  **Refactor/Helper:** Create or update operational helpers in `dot_local/bin`.
4.  **Synchronize:** Execute `chezmoi add` to move the refactored or newly created helpers into the version-controlled repository.

### Components Excluded from Current Guidance
For the purpose of initial maintenance and refactoring tasks, the following directories are intentionally excluded to keep the focus sharp:
*   **`devices/`**: Hardware-specific documentation is static and is not part of the core operational flow.
*   **`discoveries/`**: Isolated, non-production exploration projects that do not impact the system state.

### Commands
*   `chezmoi init`: Initializes the configuration repository.
*   `chezmoi apply -k`: Executes the deployment of configurations to the host.
*   `chezmoi add <path>`: Used to synchronize local helpers and configuration scripts into the version-controlled `dot_local/bin`.