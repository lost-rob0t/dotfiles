{ config, lib, pkgs, ... }:

let
  cfg = config.services.llm-log;
  alertScript = pkgs.writeShellApplication {
    name = "llm-log-quant-alerts";
    runtimeInputs = with pkgs; [ coreutils jq libnotify ];
    text = ''
      set -euo pipefail

      alerts=${lib.escapeShellArg "${cfg.dataDir}/alerts.jsonl"}
      declare -A last_seen=()

      tail -n 0 -F "$alerts" 2>/dev/null | while IFS= read -r line; do
        category=$(jq -er '.category // empty' <<<"$line") || continue
        case "$category" in
          low_quantization|possible_quantization_or_model_anomaly) ;;
          *) continue ;;
        esac

        model=$(jq -r '.model // "unknown-model"' <<<"$line")
        provider=$(jq -r '.selected_provider // .provider // "unknown-provider"' <<<"$line")
        quant=$(jq -r '.quantization // "unknown"' <<<"$line")
        detectors=$(jq -r '(.detectors // []) | join(",")' <<<"$line")
        signature="$category|$model|$provider|$quant|$detectors"
        now=$(date +%s)
        previous=''${last_seen[$signature]:-0}
        if (( now - previous < 60 )); then
          continue
        fi
        last_seen[$signature]=$now

        case "$category" in
          low_quantization)
            title="llm-log: low quantization detected"
            ;;
          *)
            title="llm-log: possible quant/model anomaly"
            ;;
        esac

        body="$model • $provider • quant=$quant"
        if [[ -n "$detectors" ]]; then
          body="$body • $detectors"
        fi
        notify-send -u critical -a llm-log "$title" "$body"
      done
    '';
  };
in
{
  config = lib.mkIf config.llm.enable {
    # Observe only. Quantization routing/enforcement is intentionally NOT enabled
    # in these dotfiles by default; this fragment only turns on anomaly detection.
    xdg.configFile."llm-log/init.d/20-quant-detection.toml".text = ''
      [quant_detection]
      enabled = true
      low_quantizations = ["fp4", "int4"]
      alert_unknown_on_anomaly = true
      repetition_window = 8
      repetition_repeats = 4
    '';

    systemd.user.services.llm-log-quant-alerts = {
      Unit = {
        Description = "Dunst alerts for llm-log quantization anomalies";
        After = [ "llm-log.service" "graphical-session.target" ];
        Wants = [ "llm-log.service" ];
      };
      Service = {
        ExecStart = "${alertScript}/bin/llm-log-quant-alerts";
        Restart = "always";
        RestartSec = 2;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
