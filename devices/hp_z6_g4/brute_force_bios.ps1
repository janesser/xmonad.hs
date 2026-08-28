# Brute Force BIOS Password Validator

# Configuration
$CharSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
$MinLength = 4
$MaxLength = 8
$Command = "BIOSConfigUtil64.exe /GetConfig /cspassfile:temp_pass.txt"
$RefusingString = "Invalid password" # Change this to the string that indicates failure
$StateFile = ".bios_brute_state"
$PasswordFile = "successful_password.txt"

function Get-NextPassword {
    param (
        [string]$CurrentPassword,
        [string]$Charset,
        [int]$TargetLength
    )

    $chars = $Charset.ToCharArray()
    $len = $CurrentPassword.Length
    if ($len -eq 0) { return $null }

    $pwdArr = $CurrentPassword.ToCharArray()
    $idx = $len - 1
    
    # Find the last character that can be incremented
    while ($idx -ge 0) {
        $currentCh = $pwdArr[$idx]
        $charIdx = [array]::IndexOf($chars, $currentCh)
        if ($charIdx -lt $chars.Length - 1) {
            $pwdArr[$idx] = $chars[$charIdx + 1]
            # Reset subsequent characters
            for ($i = $idx + 1; $i -lt $len; $i++) {
                $pwdArr[$i] = $chars[0]
            }
            return -join $pwdArr
        }
        $idx--
    }

    return $null
}

# --- Main ---

# 1. Load State
$lastTried = ""
if (Test-Path $StateFile) {
    $lastTried = (Get-Content $StateFile -Raw).Trim()
    Write-Host "[*] Resuming from last state: '$lastTried'" -ForegroundColor Cyan
}

for ($len = $MinLength; $len -le $MaxLength; $len++) {
    Write-Host "[*] Testing length: $len" -ForegroundColor Gray

    # Determine starting point for this length
    $currentAttempt = ""
    
    # If we are resuming and the last tried password matches this length
    if ($lastTried.Length -eq $len) {
        $currentAttempt = Get-NextPassword -CurrentPassword $lastTried -Charset $CharSet -TargetLength $len
        if ($null -eq $currentAttempt) {
            # We already finished this length in a previous run
            # (This can happen if the user stopped exactly after a successful end-of-length)
            # Or if the lastTried was the last possible password for this length.
            # We'll check if we should skip. 
            # Actually, if Get-NextPassword returns null, it means $lastTried was the LAST possible password.
            # So we move to next length.
            continue 
        }
        # If it's not null, we start with the NEXT one.
        # Wait, I need to check if the $lastTried WAS the successful password.
        # But we only write $lastTried if it's a guess.
    } else {
        # Start from the first possible for this length
        $chars = $CharSet.ToCharArray()
        $currentAttempt = -join (& { for ($i=1; $i -le $len; $i++) { $chars[0] })
    }

    # If lastTried was actually from a PREVIOUS length, we need to skip this length if it's already done
    # But in this simple implementation, we just start from the beginning of the length.
    # If we want to be super robust, we'd use a single index, but for BIOS lengths (usually < 10) this is fine.

    # If we are resuming and the lastTried is longer than current length, 
    # it means we finished this length.
    if ($lastTried.Length -gt $len) {
         # This is complex. Let's assume if $lastTried.Length > $len, 
         # we have already passed this length in a previous session.
         # However, the user might have restarted with a different $MinLength.
         # For safety, let's just assume if length is less than lastTried, we've already done it.
         # Or better, we check if the lastTried starts with a sequence that is "greater" than anything for this length.
         # To keep it simple: we assume the user only resumes within the same length or after.
         continue
    }

    # Actually, let's check if we are starting from a password that is already "done"
    # This is a bit complex for a single script, so let's stick to the "Resuming" logic:
    # We start from $lastTried and try to find the NEXT one.
    if ($lastTried.Length -eq $len) {
         # We need to start testing from the password AFTER $lastTried.
         # But $currentAttempt is already set to the next one by the logic above.
    } else {
        # If we aren't in a resumption state for this specific length,
        # we check if $lastTried was actually a longer password.
        # If so, we assume this length is done.
        if ($lastTried.Length -gt $len) {
            continue
        }
        # Otherwise, we start from the first possible for this length.
    }

    while ($currentAttempt -ne $null -and $currentAttempt -ne "") {
        $pwd = $currentAttempt
        
        Write-Host "[>] Trying: $pwd " -NoNewline
        
        # Update state
        $pwd | Out-File $StateFile -Encoding utf8
        
        # Prepare command
        $tempFile = "temp_pass.txt"
        $pwd | Out-File $tempFile -Encoding utf8
        
        try {
            $output = Invoke-Expression $Command 2>&1
            $outputString = $output | Out-String
            
            if (-not ($outputString -match $RefusingString)) {
                Write-Host "[OK]" -ForegroundColor Green
                Write-Host "`n[!] SUCCESSFUL PASSWORD FOUND: $pwd" -ForegroundColor Green -BackgroundColor Black
                $pwd | Out-File $PasswordFile
                Remove-Item $tempFile -ErrorAction SilentlyContinue
                Remove-Item $StateFile -ErrorAction SilentlyContinue
                exit 0
            } else {
                Write-Host "[X]" -ForegroundColor Red
            }
        }
        catch {
            Write-Host "[!]" -ForegroundColor Yellow
            Write-Warning "Command error: $($_.Exception.Message)"
        }
        finally {
            if (Test-Path $tempFile) { Remove-Item $tempFile -ErrorAction SilentlyContinue }
        }

        # Get next attempt for the SAME length
        $currentAttempt = Get-NextPassword -CurrentPassword $pwd -Charset $CharSet -TargetLength $len
    }
}

Write-Host "`n[i] Exhausted all combinations without success." -ForegroundColor Yellow
