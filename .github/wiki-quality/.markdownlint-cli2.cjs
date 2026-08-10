// @ts-check

"use strict";

/** @type {import("markdownlint").Rule} */
const no_h1 = {
  "names": [ "no-h1" ],
  "description": "This wiki should not use level-1 headings, as they are used for the page title.",
  "tags": [ "test", "headings" ],
  "parser": "markdownit",
  "function": function rule(params, onError) {
  const headingOpenTokens = params.parsers.markdownit.tokens.filter(
    (token) => token.type === "heading_open" && token.tag === "h1"
  );

  for (const token of headingOpenTokens) {
    onError({
      "lineNumber": token.lineNumber,
      "detail": "Level-1 headings are not allowed.",
      "context": token.line
    });
  }
  }
};


module.exports = {
  "customRules": [
    no_h1
  ],

  "config": {
    "line_length": false,
    // Some pages intentionally start with non-H1 headings.
    "first-line-h1": false,
    // Inline HTML is used for images
    "no-inline-html": {
      "allowed_elements": [
        "img",
        "br"
      ]
    },
    // We often have the same headers, for different siblings
    "no-duplicate-heading": {
      "siblings_only": true
    }
  }
}