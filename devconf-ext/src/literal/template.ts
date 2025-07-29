export const template = {
  name: "meta.template.definition.devconf",
  begin: "(@template)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(\\()",
  beginCaptures: {
    1: { name: "keyword.control.directive.use.devconf" },
    2: { name: "entity.name.function.template.devconf" },
    3: { name: "punctuation.definition.parameters.begin.devconf" }
  },
  end: "\\s*(:)",
  endCaptures: {
    0: { name: "punctuation.section.template.body.begin.devconf" }
  },
  patterns: [
    { include: "#string" },
    { include: "#number" },
    { include: "#boolean" },
    { include: "#constant" },
    {
      name: "variable.parameter.template.devconf",
      match: "\\b([a-zA-Z_][a-zA-Z]*)\\b"
    },
    {
      name: "keyword.operator.assignment.default.devconf",
      match: "="
    },
    {
      name: "punctuation.separator.parameter.devconf",
      match: ","
    },
    {
      name: "punctuation.definition.parameters.end.devconf",
      match: "\\)"
    }
  ]
};
