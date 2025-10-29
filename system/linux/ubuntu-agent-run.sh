#!/bin/bash

# Ubuntu Agent Runner
# This script parses ubuntu-agent.md and executes the check/apply logic
# to automate Ubuntu system setup

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_manual() {
    echo -e "${YELLOW}[MANUAL STEP]${NC} $1"
}

# Function to execute check command and determine if apply is needed
execute_check() {
    local check_cmd="$1"
    print_status "Running check: $check_cmd"

    # Execute check command and capture output
    local output
    output=$(eval "$check_cmd" 2>&1)

    print_status "Check result: $output"

    # Simple heuristic to determine if apply is needed
    # This looks for common "not found", "not installed", "No" patterns
    if echo "$output" | grep -qi "not found\|not installed\|No.*found\|No.*exists\|doesn't exist\|Default"; then
        return 0  # Apply needed
    elif echo "$output" | grep -qi "exists\|installed\|configured\|customized"; then
        return 1  # Already done
    else
        # If unclear, ask user or default to running apply
        print_warning "Unclear check result. Will run apply step."
        return 0  # Apply needed
    fi
}

# Function to execute apply command
execute_apply() {
    local apply_cmd="$1"
    print_status "Executing: $apply_cmd"

    if eval "$apply_cmd"; then
        print_success "Apply step completed successfully"
        return 0
    else
        print_error "Apply step failed with exit code $?"
        return 1
    fi
}

# Function to handle manual steps
handle_manual_step() {
    local manual_content="$1"
    print_manual "Manual step required:"
    echo "$manual_content"
    echo ""
    read -p "Press Enter when you have completed this manual step..."
}

# Main function to parse and execute the markdown file
run_agent() {
    local file_path="$1"

    if [[ ! -f "$file_path" ]]; then
        print_error "File not found: $file_path"
        exit 1
    fi

    print_status "Starting Ubuntu Agent execution"
    print_status "Reading from: $file_path"

    local current_section=""
    local in_check_block=false
    local in_apply_block=false
    local in_manual_block=false
    local check_cmd=""
    local apply_cmd=""
    local manual_content=""

    while IFS= read -r line; do
        # Skip empty lines and the header separator
        [[ -z "$line" || "$line" == "---" ]] && continue

        # Check for section headers
        if [[ "$line" =~ ^##[[:space:]]+(.*) ]]; then
            # Process previous section if we have one
            if [[ -n "$current_section" ]]; then
                process_section "$current_section" "$check_cmd" "$apply_cmd" "$manual_content"
            fi

            current_section="${BASH_REMATCH[1]}"
            print_status "Processing section: $current_section"
            in_check_block=false
            in_apply_block=false
            in_manual_block=false
            check_cmd=""
            apply_cmd=""
            manual_content=""
            continue
        fi

        # Check for check block
        if [[ "$line" == "**Check:**" ]]; then
            in_check_block=true
            in_apply_block=false
            in_manual_block=false
            check_cmd=""
            continue
        fi

        # Check for apply block
        if [[ "$line" == "**Apply:**" ]]; then
            in_check_block=false
            in_apply_block=true
            in_manual_block=false
            apply_cmd=""
            continue
        fi

        # Check for manual step
        if [[ "$line" == "**Manual Step:**" ]]; then
            in_check_block=false
            in_apply_block=false
            in_manual_block=true
            manual_content=""
            continue
        fi

        # Collect content based on current block
        if [[ "$in_check_block" == true && "$line" =~ ^\`\`\`bash ]]; then
            # Start of bash code block in check
            check_cmd=""
            continue
        elif [[ "$in_check_block" == true && "$line" == "```" && -n "$check_cmd" ]]; then
            # End of bash code block in check
            in_check_block=false
            continue
        elif [[ "$in_check_block" == true && "$line" =~ ^\`\`\` ]]; then
            # Skip the opening ``` line
            continue
        elif [[ "$in_check_block" == true ]]; then
            # Collect check command lines
            check_cmd="$check_cmd
$line"
        fi

        if [[ "$in_apply_block" == true && "$line" =~ ^\`\`\`bash ]]; then
            # Start of bash code block in apply
            apply_cmd=""
            continue
        elif [[ "$in_apply_block" == true && "$line" == "```" && -n "$apply_cmd" ]]; then
            # End of bash code block in apply
            in_apply_block=false
            continue
        elif [[ "$in_apply_block" == true && "$line" =~ ^\`\`\` ]]; then
            # Skip the opening ``` line
            continue
        elif [[ "$in_apply_block" == true ]]; then
            # Collect apply command lines
            apply_cmd="$apply_cmd
$line"
        fi

        if [[ "$in_manual_block" == true ]]; then
            if [[ "$line" =~ ^\`\`\` ]]; then
                # End of manual block
                in_manual_block=false
                handle_manual_step "$manual_content"
                manual_content=""
            else
                # Collect manual content
                manual_content="$manual_content
$line"
            fi
        fi

    done < "$file_path"

    # Process the last section
    if [[ -n "$current_section" ]]; then
        process_section "$current_section" "$check_cmd" "$apply_cmd" "$manual_content"
    fi

    print_success "Ubuntu Agent execution completed!"
}

# Function to process a complete section
process_section() {
    local section="$1"
    local check_cmd="$2"
    local apply_cmd="$3"
    local manual_content="$4"

    # Clean up commands (remove leading/trailing whitespace)
    check_cmd=$(echo "$check_cmd" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    apply_cmd=$(echo "$apply_cmd" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

    if [[ -n "$manual_content" ]]; then
        handle_manual_step "$manual_content"
    elif [[ -n "$check_cmd" && -n "$apply_cmd" ]]; then
        if execute_check "$check_cmd"; then
            print_status "Check indicates apply step is needed"
            if ! execute_apply "$apply_cmd"; then
                print_error "Failed to execute apply step for section: $section"
                exit 1
            fi
        else
            print_success "Check indicates section is already configured: $section"
        fi
    else
        print_warning "No check/apply logic found for section: $section"
    fi
}

# Main execution
main() {
    local file_path="${1:-public/system/linux/ubuntu-agent.md}"

    # Check if file exists
    if [[ ! -f "$file_path" ]]; then
        print_error "Ubuntu agent file not found: $file_path"
        echo "Usage: $0 [path_to_ubuntu_agent_file]"
        exit 1
    fi

    # Confirm execution
    echo "This will run the Ubuntu agent from: $file_path"
    read -p "Continue? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        print_status "Execution cancelled"
        exit 0
    fi

    run_agent "$file_path"
}

# Run main function with all arguments
main "$@"
