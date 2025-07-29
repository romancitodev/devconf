import { writeFileSync } from "fs";
import { devconfSyntax } from "./literal";

writeFileSync(
  "./syntaxes/devconf.tmLanguage.json",
  JSON.stringify(devconfSyntax, null, 2)
);
console.log("devconf.tmLanguage.json generado correctamente.");
