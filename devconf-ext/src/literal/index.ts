import { comment } from "./comment";
import { directive } from "./directive";
import { template } from "./template";
import { keywords } from "./keywords";
import { interpolation } from "./interpolation";
import { string } from "./string";
import { number } from "./number";
import { boolean } from "./boolean";
import { operator } from "./operator";
import { brackets } from "./brackets";
import { constant } from "./constant";
import { identifier } from "./identifier";
import { array } from "../composite/array";
import { propertyDefinition } from "../composite/propertyDefinition";

export const devconfSyntax = {
  $schema: "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
  name: "devconf",
  scopeName: "source.devconf",
  patterns: [
    { include: "#comment" },
    { include: "#directive" },
    { include: "#template" },
    { include: "#propertyDefinition" },
    { include: "#keywords" },
    { include: "#interpolation" },
    { include: "#string" },
    { include: "#number" },
    { include: "#boolean" },
    { include: "#operator" },
    { include: "#brackets" },
    { include: "#array" },
    { include: "#constant" },
    { include: "#identifier" }
  ],
  repository: {
    comment,
    directive,
    template,
    keywords,
    interpolation,
    string,
    number,
    boolean,
    operator,
    brackets,
    array,
    constant,
    identifier,
    propertyDefinition
  }
};
