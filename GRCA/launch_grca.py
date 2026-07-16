from __future__ import annotations

import importlib.util
import os
import sys
from pathlib import Path


os.environ.pop("OPENSSL_FORCE_FIPS_MODE", None)


try:
    SCRIPT_DIR = Path(__file__).resolve().parent
except NameError:
    SCRIPT_DIR = Path.cwd().resolve()


GRCA_WORKSPACE_DIRS = [
    SCRIPT_DIR,
    Path("/Workspace/Users/daniel.lang@dot.gov/Public-Lands-Computer-Vision/IP3/GRCA"),
    Path("/Workspace/Users/daniel.lang@dot.gov/Public-Lands-Computer-Vision/GRCA"),
    Path("/Workspace/Repos/daniel.lang@dot.gov/Public-Lands-Computer-Vision/IP3/GRCA"),
    Path("/Workspace/Repos/daniel.lang@dot.gov/Public-Lands-Computer-Vision/GRCA"),
]


def configure_environment() -> None:
    os.environ.pop("OPENSSL_FORCE_FIPS_MODE", None)
    try:
        os.environ["GITHUB_TOKEN"] = dbutils.secrets.get("tokens", "github_token")
    except NameError:
        token = os.getenv("GITHUB_TOKEN")
        if token:
            os.environ["GITHUB_TOKEN"] = token

    os.environ["PUBLISH_TO_GITHUB"] = "1"
    os.environ["MODEL_PATH"] = "yolov8m.pt"
    os.environ["YOLO_CONFIDENCE"] = "0.50"
    os.environ["YOLO_IOU"] = "0.45"


def add_workspace_paths() -> None:
    for directory in GRCA_WORKSPACE_DIRS:
        directory_text = str(directory)
        if directory_text not in sys.path:
            sys.path.insert(0, directory_text)


def load_count_grca_main():
    for directory in GRCA_WORKSPACE_DIRS:
        module_path = directory / "count_grca.py"
        if not module_path.exists():
            continue

        spec = importlib.util.spec_from_file_location("count_grca", module_path)
        if spec is None or spec.loader is None:
            continue

        module = importlib.util.module_from_spec(spec)
        sys.modules["count_grca"] = module
        spec.loader.exec_module(module)
        return module.main

    searched = ", ".join(str(directory / "count_grca.py") for directory in GRCA_WORKSPACE_DIRS)
    raise ModuleNotFoundError(f"Unable to find count_grca.py. Searched: {searched}")


def main() -> int:
    configure_environment()
    add_workspace_paths()

    run_main = load_count_grca_main()
    return int(run_main())


if __name__ == "__main__":
    exit_code = main()
    if exit_code:
        raise SystemExit(exit_code)
