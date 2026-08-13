#!/usr/bin/env python3
"""Synchronize reviewed Japanese proposals without reformatting source YAML."""

from __future__ import annotations

import argparse
import hashlib
import re
from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parent.parent


def plain_scalar(value: str) -> str:
    """Return a compact YAML scalar, quoting only when plain style is unsafe."""
    if "\n" not in value:
        try:
            loaded = yaml.safe_load(f"value: {value}\n")["value"]
        except yaml.YAMLError:
            loaded = None
        if loaded == value:
            return value
    return "'" + value.replace("'", "''") + "'"


def part_values(review: dict) -> list[str | None]:
    values = [review["meta"]["part_title"]["ja"]]
    for scene in review["scenes"]:
        values.append(scene["title"]["ja"])
        for entry in scene["entries"]:
            values.extend(
                variant["ja_proposed"]
                for variant in entry["translation"]["variants"]
            )
    return values


def sync_part(part: str) -> None:
    source_path = ROOT / f"part-{part}.yaml"
    review_path = ROOT / "review" / f"part-{part}.yaml"
    review = yaml.safe_load(review_path.read_text(encoding="utf-8"))
    values = part_values(review)
    lines = source_path.read_text(encoding="utf-8").splitlines(keepends=True)
    positions = [i for i, line in enumerate(lines) if re.match(r"^\s+ja:", line)]
    if len(positions) != len(values):
        raise ValueError(
            f"{source_path.name}: found {len(positions)} ja fields, expected {len(values)}"
        )
    for position, value in zip(positions, values):
        if value is None:
            continue
        indent = lines[position][: len(lines[position]) - len(lines[position].lstrip())]
        lines[position] = f"{indent}ja: {plain_scalar(value)}\n"
    source_path.write_text("".join(lines), encoding="utf-8")


def refresh_review_current(part: str) -> None:
    """Refresh review source/current Japanese fields from the main part file."""
    source_path = ROOT / f"part-{part}.yaml"
    review_path = ROOT / "review" / f"part-{part}.yaml"
    source = yaml.safe_load(source_path.read_text(encoding="utf-8"))
    values = [
        variant["ja"]
        for section in source["sections"]
        for conversation in section["conversations"]
        for variant in conversation["modern"]
    ]
    lines = review_path.read_text(encoding="utf-8").splitlines(keepends=True)
    source_positions = [
        i for i, line in enumerate(lines) if re.match(r"^        ja:", line)
    ]
    current_positions = [
        i for i, line in enumerate(lines) if re.match(r"^        ja_current:", line)
    ]
    fingerprint_positions = [
        i for i, line in enumerate(lines) if re.match(r"^      fingerprint:", line)
    ]
    fingerprints = []
    for section in source["sections"]:
        for conversation in section["conversations"]:
            payload = yaml.safe_dump(
                {
                    "modern": conversation.get("modern", []),
                    "grc": conversation.get("grc", []),
                },
                allow_unicode=True,
                sort_keys=True,
            )
            fingerprints.append(hashlib.sha256(payload.encode("utf-8")).hexdigest()[:16])
    if len(source_positions) != len(values) or len(current_positions) != len(values):
        raise ValueError(
            f"{review_path.name}: found {len(source_positions)} source and "
            f"{len(current_positions)} current fields, expected {len(values)}"
        )
    if len(fingerprint_positions) != len(fingerprints):
        raise ValueError(
            f"{review_path.name}: found {len(fingerprint_positions)} fingerprints, "
            f"expected {len(fingerprints)}"
        )
    for positions in (source_positions, current_positions):
        for position, value in zip(positions, values):
            key = "ja_current" if positions is current_positions else "ja"
            lines[position] = f"        {key}: {plain_scalar(value)}\n"
    for position, value in zip(fingerprint_positions, fingerprints):
        lines[position] = f"      fingerprint: {value}\n"
    review_path.write_text("".join(lines), encoding="utf-8")


def sync_lexicon() -> None:
    source_path = ROOT / "lexicon.yaml"
    review_path = ROOT / "review" / "lexicon.yaml"
    review = yaml.safe_load(review_path.read_text(encoding="utf-8"))
    values = [
        entry["translation"]["ja_proposed"]
        for section in review["sections"]
        for entry in section["entries"]
    ]
    lines = source_path.read_text(encoding="utf-8").splitlines(keepends=True)

    # The description is a block scalar, so replace or insert it separately.
    description = review["meta"]["source_description"]["ja_proposed"].splitlines()
    sections_at = next(i for i, line in enumerate(lines) if line == "sections:\n")
    heading_ja = next(
        (i for i, line in enumerate(lines[:sections_at]) if line.startswith("  ja:")), None
    )
    if heading_ja is not None:
        end = heading_ja + 1
        while end < sections_at and lines[end].startswith("    "):
            end += 1
        del lines[heading_ja:end]
        sections_at -= end - heading_ja
    insert = ["  ja: >-\n", *(f"    {line}\n" for line in description)]
    lines[sections_at:sections_at] = insert

    entry = 0
    index = sections_at + len(insert) + 1
    while index < len(lines):
        if lines[index].startswith("        grc:"):
            if entry >= len(values):
                raise ValueError("lexicon.yaml contains more entries than the review")
            previous = index - 1
            if re.match(r"^\s+ja:", lines[previous]):
                indent = lines[previous][: len(lines[previous]) - len(lines[previous].lstrip())]
                lines[previous] = f"{indent}ja: {plain_scalar(values[entry])}\n"
            else:
                lines.insert(index, f"            ja: {plain_scalar(values[entry])}\n")
                index += 1
            entry += 1
        index += 1
    if entry != len(values):
        raise ValueError(f"lexicon.yaml: synchronized {entry} entries, expected {len(values)}")
    source_path.write_text("".join(lines), encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("targets", nargs="+", choices=["d", "e", "f", "g", "h", "i", "lexicon"])
    parser.add_argument(
        "--refresh-review-current",
        action="store_true",
        help="copy main-file Japanese into review source/current fields",
    )
    args = parser.parse_args()
    for target in args.targets:
        if args.refresh_review_current:
            if target == "lexicon":
                parser.error("--refresh-review-current does not support lexicon")
            refresh_review_current(target)
        elif target == "lexicon":
            sync_lexicon()
        else:
            sync_part(target)
        print(f"synchronized {target}")


if __name__ == "__main__":
    main()
