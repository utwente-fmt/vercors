// @ts-check

"use strict";

/** @type {import("markdownlint").Rule} */
const first_heading_h2 = {
  "names": [ "first-heading-h2" ],
  "description": "The first heading in a file of the VerCors wiki should be an h2 heading.",
  "tags": [ "test", "headings" ],
  "parser": "markdownit",
  "function": function rule(params, onError) {
    const headingOpenTokens = params.parsers.markdownit.tokens.filter(
      (token) => token.type === "heading_open"
    );

    const firstHeading = headingOpenTokens[0];
    if (firstHeading && firstHeading.tag !== "h2") {
      onError({
        "lineNumber": firstHeading.lineNumber,
        "context": firstHeading.line
      });
    }
  }
};

module.exports = {
  "customRules": [
    first_heading_h2
  ],

  "config": {
    "line_length": false,
    // We do not want H1 headings
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