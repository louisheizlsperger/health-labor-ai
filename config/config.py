"""
Config loader: merges config_example.yaml with config.local.yaml (if present),
loads .env, injects AQS credentials from env, and normalizes paths.
"""
from __future__ import annotations
import os, pathlib, copy, yaml
from dotenv import load_dotenv

PROJECT_ROOT = pathlib.Path(__file__).resolve().parents[1]

def _deep_update(base: dict, override: dict) -> dict:
    out = copy.deepcopy(base)
    for k, v in (override or {}).items():
        if isinstance(v, dict) and isinstance(out.get(k), dict):
            out[k] = _deep_update(out[k], v)
        else:
            out[k] = v
    return out

def load_config() -> dict:
    # Load secrets (optional)
    load_dotenv(PROJECT_ROOT / ".env")

    # Base + local override
    with open(PROJECT_ROOT / "config" / "config_example.yaml", "r", encoding="utf-8") as f:
        base = yaml.safe_load(f) or {}
    local_path = PROJECT_ROOT / "config" / "config.local.yaml"
    if local_path.exists():
        with open(local_path, "r", encoding="utf-8") as f:
            local = yaml.safe_load(f) or {}
        cfg = _deep_update(base, local)
    else:
        cfg = base

    # Inject AQS credentials
    aqs = cfg.setdefault("aqs", {})
    email_env = aqs.get("email_env")
    key_env   = aqs.get("key_env")
    if email_env:
        aqs["email"] = os.getenv(email_env, "")
    if key_env:
        aqs["key"]   = os.getenv(key_env,   "")

    # Normalize key paths
    proj = cfg.setdefault("project", {})
    def _abs(path_str: str, default: str) -> str:
        path = pathlib.Path(path_str or default)
        return str((PROJECT_ROOT / path).resolve())

    proj["build_output_dir"]    = _abs(proj.get("build_output_dir"),    "build/output_external_storage")
    proj["build_logs_dir"]      = _abs(proj.get("build_logs_dir"),      "build/output/logs")
    proj["analysis_output_dir"] = _abs(proj.get("analysis_output_dir"), "analysis/output")
    proj["analysis_logs_dir"]   = _abs(proj.get("analysis_logs_dir"),   "analysis/output/logs")

    data = cfg.setdefault("data", {})
    data["datastore"] = _abs(data.get("datastore", "datastore"), "datastore")

    return cfg

if __name__ == "__main__":
    import pprint
    pprint.pp(load_config())