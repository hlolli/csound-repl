// This pre-install code should only run on nix-os distros
// Its goal is to create shell to install csound-api which
// is a node module with its own native dependencies.

const os = require("os");
const path = require("path");
const child_process = require("child_process");

if ("linux" === require("os").platform()) {
    var nixosQmark;
    try {nixosQmark = child_process.execSync("uname -a");}
    catch (e) {}
    
    // console.log(nixosQmark.toString());
    if (nixosQmark && nixosQmark.toString().match("Linux nixos")) {
        console.warn("\x1b[31m",
                     "NixOs detected, running `nix-shell -p boost csound python27`.\n", '\x1b[0m');
        
        const res = child_process.spawn(
            "nix-shell", [
                "--packages",
                "csound boost python27",
                "--run",
                "\"exec npm i csound-api --rebuild\"",
                "--show-trace"
            ],
            {
                stdio: 'pipe',
                shell: true,
                cwd: process.cwd(),
                env: process.env,
            }
        );

        res.stdout.on('data', (data) => {
            console.log(`stdout: ${data}`);
        });

        res.stderr.on('data', (data) => {
            console.log(`stderr: ${data}`);
        });

        res.on('close', (code) => {
            console.log(`child process exited with code ${code}`);
        });
        
    }
}
