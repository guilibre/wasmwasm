"use strict";

const fs = require("fs");
const { spawnSync } = require("child_process");

const [, , runnerJsPath, scorePath, expectPath] = process.argv;

if (!runnerJsPath || !scorePath || !expectPath) {
    console.error(
        "usage: node run_case.js <score_test_runner.js> <fixture.score> <fixture.expect>"
    );
    process.exit(1);
}

const scoreSource = fs.readFileSync(scorePath, "utf8");
const expectText = fs.readFileSync(expectPath, "utf8");
const stdin = `${Buffer.byteLength(scoreSource, "utf8")}\n${scoreSource}${expectText}`;

const result = spawnSync(
    process.execPath,
    ["--experimental-wasm-exnref", runnerJsPath],
    { input: stdin, stdio: ["pipe", "inherit", "inherit"] }
);

if (result.error) {
    console.error(result.error.message);
    process.exit(1);
}

process.exit(result.status === null ? 1 : result.status);
