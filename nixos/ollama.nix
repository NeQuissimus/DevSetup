{ pkgs, ... }:
{
  services.ollama = {
    enable = true;

    environmentVariables = {
      OLLAMA_CONTEXT_LENGTH = "64000";
    };

    loadModels = [ "gemma4" "qwen3.8" ];
    package = pkgs.ollama-cuda;
  };
}
