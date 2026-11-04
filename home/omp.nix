{ pkgs, lib, strataPort, ... }:
{
  home.file.".omp/agent/models.yml" = lib.mkIf (strataPort != null) {
    source = pkgs.writeText "omp-models" ''
      providers:
        strata:
          baseUrl: http://127.0.0.1:${toString strataPort}/v1
          api: openai-completions
          auth: none
          compat:
            supportsDeveloperRole: false
          models:
            - id: swift-1.5
              name: Qwen3.8-Flash-Next (Strata, local)
              reasoning: true
              input: [text]
              contextWindow: 32786
              maxTokens: 4096
              thinking:
                mode: effort
                efforts: [minimal, low, medium, high]
                defaultLevel: medium
              cost: {input: 0, output: 0, cacheRead: 0, cacheWrite: 0}
      modelRoles:
        default: strata/swift-1.5
      defaultThinkingLevel: medium
      compaction:
        enabled: true
        thresholdTokens: 15000
        keepRecentTokens: 4000
        methodOrder:
          - soft
        idleEnabled: false
      setupVersion: 2
    '';
    force = true;
  };
}
