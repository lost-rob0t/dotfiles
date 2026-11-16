zara_e2e_stage(handshake, insufficient_for_voice_readiness).
zara_e2e_stage(microphone_capture, requires_signal_not_only_active_unit).
zara_e2e_stage(voice_input, requires_real_speech_not_tone_fixtures).
zara_e2e_stage(tts, requires_nonempty_pcm_and_audio_completion).
zara_voice_failure('12f202ba31bed0a0c641e45a57fbfb3a14a582ac', vulkan,
    'RuntimeVoiceIngress._default_transcriber_factory',
    'Transcriber.normalize_device rejects vulkan although wake supports it').
zara_tts_invariant(preserve_configured_provider_without_user_approval).
zara_voice_failure('034d6b23077b543cb5d2ed8b177411bd5179ac4a', tts_disabled,
    'zmq_transport._handle_audio_input -> voice_runtime.commit:217 put_nowait',
    'queue.Full, utterance dropped as terminal stream failure (github lost-rob0t/zara#1492; 13x/90min evidence 2026-11-03)').
