"""Protocol regression tests — validate messages against JSON schemas."""

import json
from pathlib import Path

import abq.core as _core
import pytest
from abq import channel_create, init, recv, respond, send

SCHEMA_DIR = Path(__file__).parent.parent / "schemas"


def _load_schema(name: str) -> dict:
    schema_file = SCHEMA_DIR / f"{name}.schema.json"
    return json.loads(schema_file.read_text())


def _validate(instance: dict, schema: dict) -> list[str]:
    """Validate instance against JSON schema, return list of errors.

    Uses a minimal validator that doesn't require jsonschema library.
    Checks required fields, types, enums, const, pattern, and additionalProperties.
    """
    errors = []
    props = schema.get("properties", {})
    required = schema.get("required", [])

    # Check required fields
    for field in required:
        if field not in instance:
            errors.append(f"Missing required field: {field}")

    # Check each field
    for field, value in instance.items():
        if field not in props:
            if schema.get("additionalProperties") is False:
                errors.append(f"Unexpected field: {field}")
            continue

        field_schema = props[field]

        # Type check
        expected_type = field_schema.get("type")
        if expected_type:
            type_map = {
                "string": str,
                "integer": int,
                "object": dict,
                "array": list,
                "boolean": bool,
            }
            if isinstance(expected_type, list):
                # Union type like ["string", "null"]
                allowed = tuple(type_map[t] for t in expected_type if t != "null")
                if value is not None and not isinstance(value, allowed):
                    actual = type(value).__name__
                    errors.append(f"Field {field}: expected {expected_type}, got {actual}")
            elif expected_type in type_map:
                if not isinstance(value, type_map[expected_type]):
                    actual = type(value).__name__
                    errors.append(f"Field {field}: expected {expected_type}, got {actual}")

        # Const check
        if "const" in field_schema and value != field_schema["const"]:
            errors.append(f"Field {field}: expected {field_schema['const']!r}, got {value!r}")

        # Enum check
        if "enum" in field_schema and value not in field_schema["enum"]:
            errors.append(f"Field {field}: {value!r} not in {field_schema['enum']}")

        # Pattern check
        if "pattern" in field_schema and isinstance(value, str):
            import re

            if not re.match(field_schema["pattern"], value):
                errors.append(f"Field {field}: {value!r} doesn't match {field_schema['pattern']}")

        # Nested object validation
        if expected_type == "object" and isinstance(value, dict) and "properties" in field_schema:
            nested_errors = _validate(value, field_schema)
            errors.extend(f"{field}.{e}" for e in nested_errors)

    return errors


@pytest.fixture(autouse=True)
def setup_abq_home(tmp_path, monkeypatch):
    """Ensure ABQ is initialized with an isolated home for each test."""
    monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
    monkeypatch.setenv("ABQ_HOME", str(tmp_path))
    init()
    yield tmp_path


class TestRequestSchema:
    """Validate that send() produces schema-compliant requests."""

    def test_request_matches_schema(self, setup_abq_home):
        channel_create("schema-test")
        msg = send("schema-test", "signal", '{"test": true}')
        schema = _load_schema("request")
        errors = _validate(msg, schema)
        assert errors == [], f"Schema violations: {errors}"

    def test_request_all_types(self, setup_abq_home):
        schema = _load_schema("request")
        for msg_type in ["eval", "command", "signal", "status"]:
            channel_create(f"type-{msg_type}")
            msg = send(f"type-{msg_type}", msg_type, '{"type_test": true}')
            errors = _validate(msg, schema)
            assert errors == [], f"Type {msg_type} violations: {errors}"

    def test_request_with_reply_to(self, setup_abq_home):
        channel_create("reply-test")
        msg = send("reply-test", "signal", "{}", reply_to="req_abc123")
        schema = _load_schema("request")
        errors = _validate(msg, schema)
        assert errors == [], f"Schema violations: {errors}"
        assert msg["reply_to"] == "req_abc123"

    def test_request_id_format(self, setup_abq_home):
        channel_create("id-test")
        msg = send("id-test", "signal", "{}")
        assert msg["id"].startswith("req_")
        assert len(msg["id"]) > 4

    def test_request_version_pinned(self, setup_abq_home):
        channel_create("version-test")
        msg = send("version-test", "signal", "{}")
        assert msg["version"] == "1.0.0"

    def test_request_content_is_string(self, setup_abq_home):
        channel_create("content-test")
        msg = send("content-test", "signal", '{"key": "value"}')
        assert isinstance(msg["content"], str)

    def test_request_no_extra_fields(self, setup_abq_home):
        channel_create("extra-test")
        msg = send("extra-test", "signal", "{}")
        schema = _load_schema("request")
        allowed = set(schema["properties"].keys())
        actual = set(msg.keys())
        extra = actual - allowed
        assert extra == set(), f"Unexpected fields: {extra}"


class TestResponseSchema:
    """Validate that respond() produces schema-compliant responses."""

    def test_response_matches_schema(self, setup_abq_home):
        channel_create("resp-test")
        msg = send("resp-test", "signal", "{}")
        recv("resp-test")
        respond("resp-test", msg["id"], status="success", result="ok")
        schema = _load_schema("response")

        # Read the response file directly
        resp_file = setup_abq_home / "channels" / "resp-test" / "responses" / f"{msg['id']}.json"
        response = json.loads(resp_file.read_text())
        errors = _validate(response, schema)
        assert errors == [], f"Schema violations: {errors}"

    def test_response_status_enum(self, setup_abq_home):
        schema = _load_schema("response")
        valid_statuses = schema["properties"]["status"]["enum"]
        assert set(valid_statuses) == {"success", "error", "pending"}

    def test_response_version_matches_request(self, setup_abq_home):
        channel_create("ver-match")
        msg = send("ver-match", "signal", "{}")
        recv("ver-match")
        respond("ver-match", msg["id"])

        resp_file = setup_abq_home / "channels" / "ver-match" / "responses" / f"{msg['id']}.json"
        response = json.loads(resp_file.read_text())
        assert response["version"] == msg["version"]


class TestProtocolRoundtrip:
    """End-to-end protocol tests: send → recv → respond."""

    def test_roundtrip_preserves_content(self, setup_abq_home):
        channel_create("roundtrip")
        content = '{"signal": "test_complete", "passed": 52}'
        sent = send("roundtrip", "signal", content)
        received = recv("roundtrip")
        assert received is not None
        assert received["content"] == content
        assert received["id"] == sent["id"]
        assert received["type"] == "signal"

    def test_roundtrip_preserves_all_fields(self, setup_abq_home):
        channel_create("fields-rt")
        sent = send("fields-rt", "command", '{"cmd": "run"}')
        received = recv("fields-rt")
        assert received is not None
        for field in ["id", "version", "type", "from", "to", "content", "timestamp", "ttl"]:
            assert field in received, f"Missing field after recv: {field}"
            assert received[field] == sent[field], (
                f"Field {field} changed: {sent[field]} → {received[field]}"
            )
