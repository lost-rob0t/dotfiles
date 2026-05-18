#!/usr/bin/env bash
set -euo pipefail

runs="${1:-3}"
group="${QTILE_BENCH_GROUP:-0}"

for run in $(seq 1 "$runs"); do
    wrapper="$(mktemp)"
    output="$(mktemp)"

    cat >"$wrapper" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
output="$1"
{
    echo "START $(date +%s.%N)"
    time -p timeout 45s emacs --eval "(add-hook 'emacs-startup-hook (lambda () (kill-emacs)))"
    echo "END $(date +%s.%N)"
} >"$output" 2>&1
EOF
    chmod +x "$wrapper"

    qtile run-cmd -g "$group" "$wrapper" "$output"

    for _ in $(seq 1 90); do
        if rg -q '^END ' "$output" 2>/dev/null; then
            break
        fi
        sleep 0.5
    done

    echo "RUN $run"
    rg '^(real|user|sys|START|END)' "$output" || true
    rm -f "$wrapper" "$output"
done
