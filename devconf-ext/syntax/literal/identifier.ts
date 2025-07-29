export const identifier = {
  patterns: [
    {
      name: "support.type.property-name.devconf",
      match: "(?<![\\w.])[a-zA-Z_][a-zA-Z0-9_]*(?=\\.|\\s*:)"
    },
    {
      name: "punctuation.accessor.dot.devconf",
      match: "\\."
    },
    {
      name: "support.type.property-name.nested.devconf",
      match: "(?<=\\.)[a-zA-Z_][a-zA-Z0-9_\\-]*(?=\\.|\\s*:)"
    },
    {
      name: "variable.other.readwrite.devconf",
      match: "[a-zA-Z_][a-zA-Z0-9_]*"
    },
    {
      name: "string.quoted.double.property.devconf",
      match: "\"[a-zA-Z0-9_\\-]+\"(?=\\s*\\.)"
    }
  ]
};
