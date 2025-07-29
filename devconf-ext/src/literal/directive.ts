export const directive = {
  patterns: [
    {
      name: "meta.directive.use.devconf",
      begin: "(@use)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(\\()",
      beginCaptures: {
        1: { name: "keyword.control.directive.use.devconf" },
        2: { name: "entity.name.function.directive.devconf" },
        3: { name: "punctuation.definition.parameters.begin.devconf" }
      },
      end: "\\)",
      endCaptures: {
        0: { name: "punctuation.definition.parameters.end.devconf" }
      },
      patterns: [
        { include: "#string" },
        { include: "#number" },
        { include: "#boolean" },
        { include: "#constant" },
        {
          name: "punctuation.separator.parameter.devconf",
          match: ","
        }
      ]
    },
    {
      name: "meta.directive.if.devconf",
      begin: "(@if)\\s+",
      beginCaptures: {
        1: { name: "keyword.control.conditional.if.devconf" }
      },
      end: "\\s*(:)",
      endCaptures: {
        1: { name: "punctuation.section.block.begin.devconf" }
      },
      patterns: [
        { include: "#interpolation" },
        { include: "#operator" },
        { include: "#string" },
        { include: "#number" },
        { include: "#boolean" },
        { include: "#constant" }
      ]
    },
    {
      name: "meta.directive.else.devconf",
      match: "(@else)\\s*(:)",
      captures: {
        1: { name: "keyword.control.conditional.else.devconf" },
        2: { name: "punctuation.section.block.begin.devconf" }
      }
    }
  ]
};
