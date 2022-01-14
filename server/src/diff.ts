// Contains code derived from elm-language-server (https://github.com/elm-tooling/elm-language-server),
// copyright 2019 Kolja Lampe, MIT License.
import { fileURLToPath, pathToFileURL } from "url";
import { Range, TextEdit } from "vscode-languageserver-types";
import diff from "fast-diff";

// Given two strings (`before`, `after`), return a list of all substrings
// that appear in `after` but not in `before`, and the positions of each
// of the substrings within `after`.
//
// Adapted from elm-language-server:
// https://github.com/elm-tooling/elm-language-server/blob/bb7f785ebccd0cbe36d8103f8cb3feb45c7056bb/src/util/diff.ts
export function getTextRangeChanges(
  before: string,
  after: string,
): TextEdit[] {
  const newRanges: TextEdit[] = [];
  let lineNumber = 0;
  let column = 0;

  const parts = diff(before, after);

  // Loop over every part, keeping track of:
  // 1. The current line no. and column in the `after` string
  // 2. Character ranges for all "added" parts in the `after` string
  parts.forEach((part) => {
    const startLineNumber = lineNumber;
    const startColumn = column;
    if (part[0] === 0 || part[0] === -1) {
      // Split the part into lines. Loop through these lines to find
      // the line no. and column at the end of this part.
      const lines = part[1].split("\n").map((a) => a.length);
      lines.forEach((lineLength, lineIndex) => {
        // The first `line` is actually just a continuation of the last line
        if (lineIndex === 0) {
          column += lineLength;
          // All other lines come after a line break.
        } else if (lineIndex > 0) {
          lineNumber += 1;
          column = lineLength;
        }
      });
    }

    if (part[0] === 1) {
      newRanges.push({
        newText: part[1],
        range: Range.create(
          startLineNumber,
          startColumn,
          startLineNumber,
          startColumn,
        ),
      });
    } else if (part[0] === -1) {
      newRanges.push({
        newText: "",
        range: Range.create(startLineNumber, startColumn, lineNumber, column),
      });
    }
  });
  return newRanges;
}
