#!/usr/bin/env sh
zara_model="tiny"
zara_mode="cpu"
zara_workers="16"
whisper_workers="16"
state="$1"
ZARA_DICTATE_PID="/tmp/zara_dictate.pid"
lockfile="/tmp/zara_dictate.lock"

launch_sst() {
    notify-send -u normal -i microphone-sensitivity-high \
        "🎤 Starting Speech-to-Text" \
        "Model: $zara_model\nWorkers: Z=$zara_workers / W=$whisper_workers"

    zara-dictate "$zara_model" "$zara_mode" "$whisper_workers" "$zara_workers" &
    pid=$!
    echo "$pid" >"$ZARA_DICTATE_PID"
    touch "$lockfile"

    notify-send -u low -i dialog-information \
        "✅ STT Online" \
        "PID: $pid"
}

kill_sst() {
    if [ ! -f "$ZARA_DICTATE_PID" ]; then
        notify-send -u low -i dialog-warning \
            "⚠️  STT Not Running" \
            "No PID file to kill."
        return
    fi

    pid=$(cat "$ZARA_DICTATE_PID")
    kill "$pid" 2>/dev/null

    rm -f "$ZARA_DICTATE_PID"
    rm -f "$lockfile"

    notify-send -u critical -i microphone-sensitivity-muted \
        "🛑 STT Stopped" \
        "Killed PID: $pid"
}

if [ "$state" = "on" ]; then
    if [ -f "$lockfile" ]; then
        notify-send "already started stt"

        exit 0
    fi
    launch_sst
fi

if [ "$state" = "off" ]; then
    if [ -f "$lockfile" ]; then
        kill_sst
    else
        echo "zara-dictate is not running, not killing"
    fi
    exit 0
fi

if [ "$state" = "toggle" ]; then
    if [ -f "$lockfile" ]; then
        kill_sst
        exit 0
    fi
    launch_sst
fi
