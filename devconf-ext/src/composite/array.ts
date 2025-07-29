export const array = {
  patterns: [
    {
      name: "meta.sequence.array.devconf",
      begin: "\\[",
      end: "\\]",
      patterns: [
        { include: "#string" },
        { include: "#number" },
        { include: "#boolean" },
        { include: "#constant" },
        { include: "#identifier" },
        { include: "#operator" },
        { include: "#brackets" },
        {
          name: "punctuation.separator.comma.devconf",
          match: ","
        }
      ]
    }
  ]
};
