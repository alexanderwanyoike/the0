// One grammar for both a Custom bot name and a bot type's kind, so every
// name upload accepts can also be deployed as `<kind>/<name>`.
export const CUSTOM_BOT_NAME = "[a-z0-9]+(?:-[a-z0-9]+)*";

export const CUSTOM_BOT_NAME_RULE =
  "must be lowercase letters and numbers, with single hyphens between them";
