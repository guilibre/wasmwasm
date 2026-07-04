"use strict";

const fs = require("fs");
const { spawnSync } = require("child_process");

const [, , runnerJsPath, wwPath, expectPath, ...extraArgs] = process.argv;

if (!runnerJsPath || !wwPath || !expectPath) {
    console.error(
        "usage: node run_case.js <ww_test_runner.js> <fixture.ww> <fixture.expect> [math.wasm]"
    );
    process.exit(1);
}

const wwSource = fs.readFileSync(wwPath, "utf8");
const expectText = fs.readFileSync(expectPath, "utf8");
const stdin = `${Buffer.byteLength(wwSource, "utf8")}\n${wwSource}${expectText}`;

const result = spawnSync(
    process.execPath,
    ["--experimental-wasm-exnref", runnerJsPath, ...extraArgs],
    { input: stdin, stdio: ["pipe", "inherit", "inherit"] }
);

if (result.error) {
    console.error(result.error.message);
    process.exit(1);
}

process.exit(result.status === null ? 1 : result.status);
