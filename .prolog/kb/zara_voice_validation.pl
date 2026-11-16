zara_e2e_stage(handshake, insufficient_for_voice_readiness).
zara_e2e_stage(microphone_capture, requires_signal_not_only_active_unit).
zara_e2e_stage(voice_input, requires_real_speech_not_tone_fixtures).
zara_e2e_stage(tts, requires_nonempty_pcm_and_audio_completion).
zara_voice_failure('12f202ba31bed0a0c641e45a57fbfb3a14a582ac', vulkan,
    'RuntimeVoiceIngress._default_transcriber_factory',
    'Transcriber.normalize_device rejects vulkan although wake supports it').
zara_tts_invariant(preserve_configured_provider_without_user_approval).
