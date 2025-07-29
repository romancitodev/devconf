export const propertyDefinition = {
  patterns: [
    {
      name: "meta.property.definition.devconf",
      begin: "((?:[a-zA-Z_][a-zA-Z0-9_]*|\"[a-zA-Z0-9_\\-]+\")(?:\\.[a-zA-Z0-9_\\-\"]+)*)\\s*(\\:)\\s*",
      beginCaptures: {
        1: {
          patterns: [
            {
              match: "([a-zA-Z_][a-zA-Z0-9_]*)",
              name: "support.type.property-name.devconf"
            },
            {
              match: "\\.([a-zA-Z_][a-zA-Z0-9_\\-]*)",
              captures: {
                1: { name: "support.type.property-name.nested.devconf" }
              }
            },
            {
              match: "\\.(\"[a-zA-Z0-9_\\-]+\")",
              captures: {
                1: { name: "string.quoted.double.property.devconf" }
              }
            }
          ]
        },
        2: { name: "punctuation.separator.key-value.devconf" }
      },
      end: "(?=$|\\s|#)",
      patterns: [
        { include: "#string" },
        { include: "#number" },
        { include: "#boolean" },
        { include: "#interpolation" },
        { include: "#constant" },
        { include: "#array" },
        { include: "#brackets" }
      ]
    }
  ]
};
