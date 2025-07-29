export const number = {
  patterns: [
    {
      name: "constant.numeric.float.devconf",
      match: "(?<![\\w.])\\d+(?:_\\d+)*\\.\\d+(?:_\\d+)*(?![\\w.])"
    },
    {
      name: "constant.numeric.integer.devconf",
      match: "(?<![\\w.])\\d+(?:_\\d+)*(?![\\w.])"
    }
  ]
};
