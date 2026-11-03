#!/usr/bin/env python3
"""On-demand macOS launchd job; no boot/login registration or draft mutations."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import plistlib
import re
import shutil
import socket
import subprocess
import sys
import time
import urllib.error
import urllib.request

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "appdata"
LABEL = "local.planteam.builder." + hashlib.sha256(str(ROOT).encode()).hexdigest()[:10]
DOMAIN = "gui/" + str(os.getuid())
TARGET = DOMAIN + "/" + LABEL
PLIST = DATA / "builder-launch.plist"
LOG = DATA / "builder-service.log"
URL = "http://127.0.0.1:3847/"
RUNNER = ROOT / "scripts" / "run-builder.R"


def launchctl(*args, required=True):
    result = subprocess.run(["/bin/launchctl", *args], text=True, capture_output=True)
    if required and result.returncode:
        raise RuntimeError((result.stderr or result.stdout).strip())
    return result


def job():
    result = launchctl("print", TARGET, required=False)
    if result.returncode:
        return None
    if str(RUNNER) not in result.stdout:
        raise RuntimeError("The service label belongs to a different command; refusing to alter it.")
    match = re.search(r"\bpid = (\d+)", result.stdout)
    return {"pid": int(match.group(1)) if match else None, "target": TARGET}


def port_open():
    with socket.socket() as connection:
        connection.settimeout(0.5)
        return connection.connect_ex(("127.0.0.1", 3847)) == 0


def healthy():
    try:
        with urllib.request.urlopen(URL, timeout=2) as response:
            return response.status == 200 and b"SAFE introduction builder" in response.read(512000)
    except (OSError, urllib.error.URLError):
        return False


def start():
    current = job()
    if port_open() and current is None:
        raise RuntimeError("Port 3847 is already in use by an unmanaged process. It was left untouched. Close that app in its own terminal before using this launcher.")
    if current is not None and healthy():
        print(json.dumps({"status": "already running", "url": URL, "pid": current["pid"], "service": TARGET, "log": str(LOG)}))
        return
    DATA.mkdir(exist_ok=True)
    if current is None:
        rscript = Path("/Library/Frameworks/R.framework/Resources/bin/Rscript")
        if not rscript.is_file():
            found = shutil.which("Rscript")
            if not found:
                raise RuntimeError("Rscript is missing; install R before starting the builder.")
            rscript = Path(found).resolve()
        config = {
            "Label": LABEL,
            "ProgramArguments": [str(rscript), "--vanilla", str(RUNNER)],
            "WorkingDirectory": str(ROOT),
            "StandardOutPath": str(LOG),
            "StandardErrorPath": str(LOG),
            "RunAtLoad": True,
            "KeepAlive": False,
            "ProcessType": "Interactive",
            "EnvironmentVariables": {
                "PATH": str(rscript.parent) + ":/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin",
                "LANG": "en_US.UTF-8",
            },
        }
        if os.environ.get("PLANTEAM_BUILDER_DATA"):
            custom_data = Path(os.environ["PLANTEAM_BUILDER_DATA"]).expanduser()
            if not custom_data.is_absolute():
                raise RuntimeError("PLANTEAM_BUILDER_DATA must be an absolute path.")
            config["EnvironmentVariables"]["PLANTEAM_BUILDER_DATA"] = str(custom_data)
        temporary = PLIST.with_suffix(".pending.plist")
        with temporary.open("wb") as stream:
            plistlib.dump(config, stream)
        temporary.replace(PLIST)
        launchctl("bootstrap", DOMAIN, str(PLIST))
    elif current["pid"] is None:
        # Never use kickstart -k: an existing active process must not be killed.
        launchctl("kickstart", TARGET)
    deadline = time.monotonic() + 20
    while time.monotonic() < deadline:
        if healthy():
            current = job()
            print(json.dumps({"status": "running", "url": URL, "pid": current["pid"], "service": TARGET, "log": str(LOG)}))
            return
        time.sleep(0.25)
    raise RuntimeError("The builder did not become ready. Your documents were not changed. Read " + str(LOG))


def stop():
    current = job()
    if current is None:
        print("This checkout's on-demand builder service is not registered. No process was stopped.")
        return
    if current["pid"] is not None:
        launchctl("kill", "SIGINT", TARGET, required=False)
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline and port_open():
            time.sleep(0.2)
    launchctl("bootout", TARGET)
    print("Stopped and unregistered this checkout's builder service. Working files and backups were retained.")


def status():
    current = job()
    ready = current is not None and healthy()
    print(json.dumps({"ready": ready, "url": URL, "service": current, "log": str(LOG)}))
    return 0 if ready else 1


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=["start", "stop", "status"])
    args = parser.parse_args()
    try:
        if args.action == "start":
            start()
        elif args.action == "stop":
            stop()
        else:
            sys.exit(status())
    except (RuntimeError, OSError) as error:
        print("Builder launcher: " + str(error), file=sys.stderr)
        sys.exit(1)
