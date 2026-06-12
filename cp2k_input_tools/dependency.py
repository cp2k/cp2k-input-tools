"""Dependency hierarchy parsing for CP2K input files.

This module provides functionality to extract and analyze dependencies
between variables, include files, and external file references in CP2K
input files.
"""

import re
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Set, Tuple


@dataclass
class DependencyNode:
    """A node in the dependency graph representing a variable, include file, or external dependency."""

    id: str
    type: str  # "variable", "include", "external"
    name: str
    value: str = ""  # Optional value for variables
    line_number: int = -1  # Source line number if available
    filename: str = ""  # Source filename if available


@dataclass
class DependencyEdge:
    """An edge in the dependency graph representing a relationship between nodes."""

    source: str  # ID of the source node
    target: str  # ID of the target node
    type: str  # "references", "includes", "requires"
    line_number: int = -1  # Source line number if available


@dataclass
class DependencyGraph:
    """A dependency graph representing relationships in CP2K input files."""

    nodes: List[DependencyNode]
    edges: List[DependencyEdge]

    def get_topological_sort(self) -> List[DependencyNode]:
        """Return a topological ordering of the dependency graph nodes.

        This is useful for determining the order in which dependencies should be
        resolved. Nodes with no dependencies come first.

        Returns:
            List[DependencyNode]: Topologically sorted list of nodes
        """
        # Build adjacency list and in-degree count
        in_degree: Dict[str, int] = {node.id: 0 for node in self.nodes}
        adj_list: Dict[str, List[str]] = defaultdict(list)

        # Create node lookup
        node_map: Dict[str, DependencyNode] = {node.id: node for node in self.nodes}

        # Build graph structure
        for edge in self.edges:
            adj_list[edge.source].append(edge.target)
            in_degree[edge.target] += 1

        # Kahn's algorithm for topological sort
        # Sort queue for deterministic ordering
        queue: List[str] = sorted([node_id for node_id, degree in in_degree.items() if degree == 0])
        result: List[DependencyNode] = []

        while queue:
            node_id = queue.pop(0)
            result.append(node_map[node_id])

            for neighbor in adj_list[node_id]:
                in_degree[neighbor] -= 1
                if in_degree[neighbor] == 0:
                    queue.append(neighbor)
            # Sort queue to maintain deterministic ordering
            queue.sort()

        return result

    def detect_cycles(self) -> List[List[str]]:
        """Detect circular dependencies in the graph.

        Returns:
            List[List[str]]: List of cycles, where each cycle is a list of node IDs
        """
        # Build adjacency list
        adj_list: Dict[str, List[str]] = defaultdict(list)
        node_ids: Set[str] = {node.id for node in self.nodes}

        for edge in self.edges:
            adj_list[edge.source].append(edge.target)

        # DFS-based cycle detection
        visited: Set[str] = set()
        rec_stack: Set[str] = set()
        cycles: List[List[str]] = []
        path: List[str] = []

        def dfs(node_id: str) -> bool:
            """DFS helper that returns True if a cycle is found."""
            visited.add(node_id)
            rec_stack.add(node_id)
            path.append(node_id)

            for neighbor in adj_list[node_id]:
                if neighbor not in visited:
                    if dfs(neighbor):
                        return True
                elif neighbor in rec_stack:
                    # Found a cycle
                    cycle_start = path.index(neighbor)
                    cycle = path[cycle_start:] + [neighbor]
                    # Normalize the cycle to start from the smallest ID for consistency
                    min_idx = cycle.index(min(cycle))
                    normalized_cycle = cycle[min_idx:-1] + cycle[:min_idx] + [cycle[min_idx]]
                    if normalized_cycle not in cycles:
                        cycles.append(normalized_cycle)

            path.pop()
            rec_stack.remove(node_id)
            return False

        # Check each node
        for node_id in node_ids:
            if node_id not in visited:
                dfs(node_id)

        return cycles

    def to_json(self) -> dict:
        """Convert the dependency graph to JSON-serializable dictionary.

        Returns:
            dict: JSON-serializable representation of the graph
        """
        return {
            "nodes": [
                {
                    "id": node.id,
                    "type": node.type,
                    "name": node.name,
                    "value": node.value,
                    "line_number": node.line_number,
                    "filename": node.filename,
                }
                for node in self.nodes
            ],
            "edges": [
                {
                    "source": edge.source,
                    "target": edge.target,
                    "type": edge.type,
                    "line_number": edge.line_number,
                }
                for edge in self.edges
            ],
        }


class DependencyExtractor:
    """Extracts dependency information from CP2K input files."""

    # Regular expressions for preprocessor directives
    _SET_MATCH = re.compile(r"\s*@SET\s+(?P<var>\S+)\s+(?P<value>.+)", flags=re.IGNORECASE)
    _INCLUDE_MATCH = re.compile(r"\s*@(?P<type>INCLUDE|XCTYPE)\b\s*(?P<file>.*)", flags=re.IGNORECASE)
    _VAR_REF_BRACED = re.compile(r"\$\{([A-Z_][A-Z0-9_]*)\}", re.IGNORECASE)
    _VAR_REF_SIMPLE = re.compile(r"\$([A-Z_][A-Z0-9_]*)\b", re.IGNORECASE)

    def __init__(self, base_dir="."):
        """Initialize the dependency extractor.

        Args:
            base_dir: Base directory for resolving include file paths
        """
        self.base_dir = Path(base_dir) if isinstance(base_dir, str) else base_dir

        # Dependency tracking
        self.variable_definitions: Dict[str, Tuple[str, int, str]] = {}  # name -> (value, line, filename)
        self.variable_references: Dict[str, List[Tuple[str, int, str]]] = defaultdict(
            list
        )  # var -> list of (ref_var, line, filename)
        self.include_files: Dict[str, Tuple[int, str]] = {}  # filename -> (line, parent_filename)
        self.include_dependencies: Dict[str, List[str]] = defaultdict(list)  # file -> list of included files
        self.external_deps: Dict[str, Tuple[str, int, str]] = {}  # keyword -> (filepath, line, filename)

        # Processing state
        self._current_file = "main"
        self._current_line = 0
        self._file_stack = ["main"]

    def extract_from_file(self, filepath: str) -> DependencyGraph:
        """Extract dependencies from a CP2K input file.

        Args:
            filepath: Path to the CP2K input file

        Returns:
            DependencyGraph: Graph representing all dependencies
        """
        filepath = Path(filepath)
        if not filepath.is_absolute():
            filepath = self.base_dir / filepath

        self._current_file = str(filepath)
        self._file_stack = [str(filepath)]

        with open(filepath, "r") as f:
            content = f.read()

        # First pass: collect all information
        self._process_content(content, str(filepath))

        # Build the graph
        return self._build_graph()

    def extract_from_content(self, content: str, filename: str = "main") -> DependencyGraph:
        """Extract dependencies from CP2K input content.

        Args:
            content: CP2K input content as string
            filename: Optional filename for error reporting

        Returns:
            DependencyGraph: Graph representing all dependencies
        """
        self._current_file = filename
        self._file_stack = [filename]

        self._process_content(content, filename)

        return self._build_graph()

    def _process_content(self, content: str, filename: str) -> None:
        """Process content to extract dependencies.

        Args:
            content: File content
            filename: Current filename being processed
        """
        lines = content.split("\n")

        for line_num, line in enumerate(lines, 1):
            self._current_line = line_num
            self._current_file = filename

            # Check for @SET directives
            set_match = self._SET_MATCH.match(line)
            if set_match:
                var_name = set_match.group("var").upper()
                var_value = set_match.group("value").strip()

                # Store variable definition
                self.variable_definitions[var_name] = (var_value, line_num, filename)

                # Extract variable references from the value
                refs = self._extract_variable_references(var_value)
                for ref in refs:
                    self.variable_references[ref].append((var_name, line_num, filename))

                continue

            # Check for @INCLUDE directives
            include_match = self._INCLUDE_MATCH.match(line)
            if include_match:
                inc_type = include_match.group("type").upper()
                inc_file = include_match.group("file").strip()

                # Remove quotes if present
                inc_file = inc_file.strip("\"'")

                if inc_type == "XCTYPE":
                    inc_file = f"xc_section/{inc_file}.sec"

                # Store include file
                self.include_files[inc_file] = (line_num, filename)

                # Track include dependency
                current_file = self._file_stack[-1] if self._file_stack else filename
                self.include_dependencies[current_file].append(inc_file)

                # Try to process the included file
                try:
                    inc_path = Path(inc_file)
                    if not inc_path.is_absolute():
                        inc_path = self.base_dir / inc_file

                    if inc_path.exists():
                        self._file_stack.append(str(inc_path))
                        with open(inc_path, "r") as f:
                            self._process_content(f.read(), str(inc_path))
                        self._file_stack.pop()
                except (OSError, IOError):
                    # File not found or cannot be read - still track the dependency
                    pass

                continue

            # Check for external file dependencies in section keywords
            # Look for patterns like: BASIS_SET_FILE_NAME path/to/file
            external_match = re.match(
                r"\s*(BASIS_SET_FILE_NAME|POTENTIAL_FILE_NAME|XYZ_FILE|RESTART_FILE_NAME|WFN_FILE_NAME)\s+(.+)", line, re.IGNORECASE
            )
            if external_match:
                keyword = external_match.group(1).upper()
                filepath = external_match.group(2).strip().strip("\"'")

                if filepath:
                    self.external_deps[keyword] = (filepath, line_num, filename)

    def _extract_variable_references(self, text: str) -> List[str]:
        """Extract variable references from a string.

        Args:
            text: String to search for variable references

        Returns:
            List[str]: List of variable names that are referenced
        """
        references = []

        # Find ${VAR} patterns
        for match in self._VAR_REF_BRACED.finditer(text):
            references.append(match.group(1).upper())

        # Find $VAR patterns (avoiding ${VAR} duplicates)
        for match in self._VAR_REF_SIMPLE.finditer(text):
            var_name = match.group(1).upper()
            # Make sure this isn't part of a ${VAR} pattern
            start_pos = match.start()
            if start_pos < 2 or text[start_pos - 2 : start_pos] != "${":
                references.append(var_name)

        return list(set(references))  # Remove duplicates

    def _build_graph(self) -> DependencyGraph:
        """Build the dependency graph from collected information.

        Returns:
            DependencyGraph: The constructed dependency graph
        """
        nodes = []
        edges = []

        # Build nodes for variables
        for var_name, (value, line, filename) in self.variable_definitions.items():
            node_id = f"var_{var_name}"
            nodes.append(
                DependencyNode(
                    id=node_id,
                    type="variable",
                    name=var_name,
                    value=value,
                    line_number=line,
                    filename=filename,
                )
            )

        # Build nodes for includes
        for filename, (line, parent_file) in self.include_files.items():
            node_id = f"include_{filename}"
            nodes.append(
                DependencyNode(
                    id=node_id,
                    type="include",
                    name=filename,
                    line_number=line,
                    filename=parent_file,
                )
            )

        # Build nodes for external dependencies
        for keyword, (filepath, line, filename) in self.external_deps.items():
            node_id = f"external_{keyword}"
            nodes.append(
                DependencyNode(
                    id=node_id,
                    type="external",
                    name=f"{keyword}:{filepath}",
                    value=filepath,
                    line_number=line,
                    filename=filename,
                )
            )

        # Build edges for variable references
        # FIXED: Edge direction should be: referenced_var -> referencing_var
        # (the dependency that must be resolved first comes first)
        for ref_var, references in self.variable_references.items():
            for ref_location, line, _filename in references:
                if ref_var in self.variable_definitions:
                    # Edge from referenced var to referencing var
                    # (ref_var must be resolved before ref_location)
                    target_id = f"var_{ref_location}"
                    source_id = f"var_{ref_var}"
                    edges.append(
                        DependencyEdge(
                            source=source_id,
                            target=target_id,
                            type="references",
                            line_number=line,
                        )
                    )

        # Build edges for include dependencies
        # FIXED: Edge direction should be: included_file -> including_file
        # (the included file must be processed before the including file)
        for including_file, included_files in self.include_dependencies.items():
            for included_file in included_files:
                target_id = f"include_{including_file}"
                source_id = f"include_{included_file}"
                edges.append(
                    DependencyEdge(
                        source=source_id,
                        target=target_id,
                        type="includes",
                        line_number=-1,
                    )
                )

        return DependencyGraph(nodes=nodes, edges=edges)


def build_dependency_graph(tree: dict, base_dir: str = ".") -> DependencyGraph:
    """Build a dependency graph from a parsed CP2K input tree.

    This is a convenience function that creates a DependencyExtractor
    and extracts dependencies. Note that this function works with the
    parsed tree, but for full dependency information including @SET
    and @INCLUDE directives, it's better to use DependencyExtractor
    directly on the input file.

    Args:
        tree: Parsed CP2K input tree from the parser
        base_dir: Base directory for resolving include paths

    Returns:
        DependencyGraph: Graph representing all dependencies
    """
    # For tree-based extraction, we can only get external dependencies
    # from keyword values
    nodes = []
    edges = []

    external_deps = {}

    def extract_from_dict(d, filename="tree"):
        if not isinstance(d, dict):
            return

        for key, value in d.items():
            if isinstance(value, str):
                # Check for external file dependencies
                if key.upper() in ("BASIS_SET_FILE_NAME", "POTENTIAL_FILE_NAME", "XYZ_FILE", "RESTART_FILE_NAME", "WFN_FILE_NAME"):
                    clean_value = value.strip("\"'")
                    if clean_value:
                        external_deps[key.upper()] = (clean_value, -1, filename)
            elif isinstance(value, dict):
                extract_from_dict(value, filename)
            elif isinstance(value, list):
                for item in value:
                    if isinstance(item, dict):
                        extract_from_dict(item, filename)

    extract_from_dict(tree)

    # Build nodes for external dependencies
    for keyword, (filepath, line, filename) in external_deps.items():
        node_id = f"external_{keyword}"
        nodes.append(
            DependencyNode(
                id=node_id,
                type="external",
                name=f"{keyword}:{filepath}",
                value=filepath,
                line_number=line,
                filename=filename,
            )
        )

    return DependencyGraph(nodes=nodes, edges=edges)
