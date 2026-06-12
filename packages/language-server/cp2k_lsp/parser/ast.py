"""AST (Abstract Syntax Tree) for CP2K input."""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, List, Optional


class ValueType(Enum):
    """Value types."""

    STRING = "string"
    NUMBER = "number"
    BOOLEAN = "boolean"
    ARRAY = "array"
    UNIT = "unit"


@dataclass
class ASTNode:
    """Base AST node."""

    line: int = 0
    column: int = 0

    def accept(self, visitor):
        method_name = f"visit_{self.__class__.__name__}"
        visitor_method = getattr(visitor, method_name, self.generic_visit)
        return visitor_method(self)

    def generic_visit(self, visitor):
        raise NotImplementedError(f"No visit method for {self.__class__.__name__}")


@dataclass
class Value(ASTNode):
    """Value node."""

    value: Any = None
    value_type: ValueType = ValueType.STRING
    unit: Optional[str] = None

    def __repr__(self) -> str:
        if self.unit:
            return f"Value({self.value} {self.unit})"
        return f"Value({self.value})"


@dataclass
class Keyword(ASTNode):
    """Keyword node."""

    name: str = ""
    value: Value = field(default_factory=lambda: Value())
    description: Optional[str] = None

    def __repr__(self) -> str:
        return f"Keyword({self.name}={self.value})"


@dataclass
class Comment(ASTNode):
    """Comment node."""

    text: str = ""

    def __repr__(self) -> str:
        return f"Comment({self.text!r})"


@dataclass
class Section(ASTNode):
    """Section node."""

    name: str = ""
    keywords: List[Keyword] = field(default_factory=list)
    subsections: List["Section"] = field(default_factory=list)
    comments: List[Comment] = field(default_factory=list)

    def get_keyword(self, name: str) -> Optional[Keyword]:
        """Get keyword by name."""
        for kw in self.keywords:
            if kw.name.upper() == name.upper():
                return kw
        return None

    def get_subsection(self, name: str) -> Optional["Section"]:
        """Get subsection by name."""
        for sub in self.subsections:
            if sub.name.upper() == name.upper():
                return sub
        return None

    def __repr__(self) -> str:
        return f"Section(&{self.name}, {len(self.keywords)} keywords, {len(self.subsections)} subsections)"


@dataclass
class CP2KInput(ASTNode):
    """Root CP2K input node."""

    global_section: Optional[Section] = None
    sections: List[Section] = field(default_factory=list)
    comments: List[Comment] = field(default_factory=list)

    def get_section(self, name: str) -> Optional[Section]:
        """Get top-level section by name."""
        if self.global_section and self.global_section.name.upper() == name.upper():
            return self.global_section
        for sec in self.sections:
            if sec.name.upper() == name.upper():
                return sec
        return None

    def __repr__(self) -> str:
        total = len(self.sections)
        if self.global_section:
            total += 1
        return f"CP2KInput({total} sections)"
