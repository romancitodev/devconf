export const interpolation = {
  patterns: [
    {
      name: "meta.interpolation.devconf",
      begin: "\\$\\{",
      beginCaptures: {
        0: { name: "punctuation.definition.template-expression.begin.devconf" }
      },
      end: "\\}",
      endCaptures: {
        0: { name: "punctuation.definition.template-expression.end.devconf" }
      },
      patterns: [
        {
          match: "([A-Z_][A-Z0-9_]*)(?=\s*(?::|==|!=|\|\||&&|\}|$))",
          name: "variable.other.constant.env.devconf"
        },
        {
          match: "\\b(int|float|bool|string)\\b",
          name: "storage.type.devconf"
        },
        {
          match: ":",
          name: "punctuation.separator.key-value.devconf"
        },
        { include: "#string" },
        { include: "#number" },
        { include: "#boolean" },
        { include: "#operator" },
        { include: "#constant" },
        { include: "#identifier" }
      ]
    }
  ]
};
