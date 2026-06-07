"""Warning tiers and classification rules for gfortran inventories."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Self

INVENTORY_FIELDNAMES = (
    "warning_type",
    "tier",
    "tier_weight",
    "file",
    "line",
    "column",
    "message",
    "compile_unit",
    "loc_file",
)

OTHER_UNKNOWN = "unknown"


class Tier(Enum):
    """Warning severity tier persisted in inventory CSVs."""

    A = ("A", 3)
    B = ("B", 1)
    C = ("C", 0)

    label: str
    weight: int

    def __new__(cls, label: str, weight: int) -> Self:
        """Create a tier member with CSV label and sort weight."""
        member = object.__new__(cls)
        member._value_ = label
        member.label = label
        member.weight = weight
        return member


@dataclass(frozen=True, slots=True)
class ClassificationRule:
    """Warning category matched by substrings in a lowered message."""

    warning_type: str
    tier: Tier
    all_of: tuple[str, ...] = ()
    any_of: tuple[str, ...] = ()

    def matches(self, message_lower: str) -> bool:
        """Return True when ``message_lower`` matches this rule."""
        if self.all_of and not all(part in message_lower for part in self.all_of):
            return False
        if self.any_of and not any(part in message_lower for part in self.any_of):
            return False
        return bool(self.all_of or self.any_of)


CLASSIFICATION_RULES: tuple[ClassificationRule, ...] = (
    ClassificationRule(
        "character_truncation", Tier.A, all_of=("character", "truncat")
    ),
    ClassificationRule(
        "type_conversion",
        Tier.A,
        any_of=("change of value in conversion", "conversion from"),
    ),
    ClassificationRule(
        "integer_division",
        Tier.A,
        all_of=("integer division", "truncat"),
    ),
    ClassificationRule(
        "uninitialized",
        Tier.A,
        any_of=("may be used uninitialized", "is used uninitialized"),
    ),
    ClassificationRule("type_mismatch", Tier.A, any_of=("type mismatch",)),
    ClassificationRule(
        "unused_dummy_argument", Tier.B, any_of=("unused dummy argument",)
    ),
    ClassificationRule(
        "unused_variable", Tier.B, any_of=("unused variable",)
    ),
    ClassificationRule(
        "unused_label", Tier.B, all_of=("label", "defined but not used")
    ),
    ClassificationRule("tab_character", Tier.B, any_of=("tab character",)),
    ClassificationRule(
        "large_stack_array",
        Tier.C,
        any_of=("fmax-stack-var-size", "moved from stack to static"),
    ),
    ClassificationRule("extension", Tier.B, any_of=("extension:",)),
    ClassificationRule("obsolescent", Tier.B, any_of=("obsolescent",)),
    ClassificationRule(
        "deleted_feature",
        Tier.B,
        any_of=("deleted feature:", "fortran 2018 deleted feature:"),
    ),
)
