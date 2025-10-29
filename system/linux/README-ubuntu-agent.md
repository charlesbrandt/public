# Ubuntu Agent Runner

The `ubuntu-agent-run.sh` script is an automated agent that can execute the Ubuntu setup plan defined in `ubuntu-agent.md`.

## Features

- **Idempotent**: Safe to run multiple times without causing issues
- **Intelligent**: Checks if steps are already completed before running them
- **Interactive**: Prompts for manual steps and confirmation
- **Colored Output**: Clear visual feedback with status indicators
- **Error Handling**: Stops execution if any step fails

## Usage

### Basic Usage

```bash
# Run the default ubuntu-agent.md file
./public/system/linux/ubuntu-agent-run.sh

# Run a specific markdown file
./public/system/linux/ubuntu-agent-run.sh /path/to/your/agent-file.md
```

### What It Does

1. **Parses** the markdown file section by section
2. **Executes** the `Check` command for each section
3. **Analyzes** the output to determine if the `Apply` step is needed
4. **Runs** the `Apply` command only if necessary
5. **Prompts** for manual steps and waits for user confirmation
6. **Reports** success/failure for each step

### Example Output

```
[INFO] Starting Ubuntu Agent execution
[INFO] Reading from: public/system/linux/ubuntu-agent.md
[INFO] Processing section: Prerequisites Check
[INFO] Running check: which git
[INFO] Check result: /usr/bin/git
[SUCCESS] Check indicates section is already configured: Prerequisites Check
[INFO] Processing section: SSH Key Generation
[INFO] Running check: ls -la ~/.ssh/id_rsa.pub 2>/dev/null | grep -q id_rsa.pub && echo "SSH key exists" || echo "No SSH key found"
[INFO] Check result: No SSH key found
[INFO] Check indicates apply step is needed
[INFO] Executing: ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
...
```

## File Structure

- `ubuntu-agent.md` - The master plan file with check/apply steps
- `ubuntu-agent-run.sh` - The agent script that executes the plan
- `README-ubuntu-agent.md` - This documentation

## Safety Features

- **Confirmation prompt** before starting execution
- **Error detection** - stops if any apply step fails
- **Idempotent operations** - checks before applying changes
- **Manual step handling** - waits for user input on manual steps

## Customization

You can create your own agent files by following the same format as `ubuntu-agent.md`:

```markdown
## Section Name

**Check:**
```bash
command_to_check_if_done
```

**Apply:**
```bash
command_to_execute_if_needed
```

---

## Manual Step

**Manual Step:**
```
Instructions for manual steps go here.
```
```

## Troubleshooting

If the script fails to parse your markdown file correctly:

1. Ensure proper markdown formatting
2. Use `**Check:**` and `**Apply:**` headers exactly
3. Use bash code blocks with triple backticks
4. Test individual check/apply commands manually first

## Integration

This agent can be integrated into:
- System provisioning scripts
- Docker containers
- CI/CD pipelines
- New machine setup automation
