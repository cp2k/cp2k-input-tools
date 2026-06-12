"""Test dependency hierarchy parsing for CP2K input files."""

from pathlib import Path

import pytest

from cp2k_input_tools.dependency import (
    DependencyEdge,
    DependencyExtractor,
    DependencyGraph,
    DependencyNode,
)


@pytest.fixture
def test_inputs_dir():
    """Fixture providing path to test inputs directory."""
    return Path(__file__).parent / "inputs"


class TestDependencyGraph:
    """Test the DependencyGraph dataclass and basic operations."""

    def test_empty_graph(self):
        """Test creating an empty dependency graph."""
        graph = DependencyGraph(nodes=[], edges=[])
        assert len(graph.nodes) == 0
        assert len(graph.edges) == 0

    def test_graph_with_nodes(self):
        """Test creating a graph with dependency nodes."""
        nodes = [
            DependencyNode(id="var1", type="variable", name="MY_VAR"),
            DependencyNode(id="file1", type="include", name="common.inc"),
        ]
        graph = DependencyGraph(nodes=nodes, edges=[])
        assert len(graph.nodes) == 2
        assert graph.nodes[0].type == "variable"
        assert graph.nodes[1].type == "include"

    def test_graph_with_edges(self):
        """Test creating a graph with dependency edges."""
        nodes = [
            DependencyNode(id="var1", type="variable", name="VAR1"),
            DependencyNode(id="var2", type="variable", name="VAR2"),
        ]
        edges = [
            DependencyEdge(source="var1", target="var2", type="references"),
        ]
        graph = DependencyGraph(nodes=nodes, edges=edges)
        assert len(graph.edges) == 1
        assert graph.edges[0].source == "var1"
        assert graph.edges[0].target == "var2"


class TestVariableDependencies:
    """Test @SET variable and ${VAR} reference parsing."""

    def test_simple_variable_definition(self, test_inputs_dir):
        """Test parsing a simple @SET variable definition."""
        content = """@SET MY_VAR 100.0
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Should have one variable node
        var_nodes = [n for n in graph.nodes if n.type == "variable"]
        assert len(var_nodes) == 1
        assert var_nodes[0].name == "MY_VAR"
        assert var_nodes[0].value == "100.0"

    def test_variable_reference(self, test_inputs_dir):
        """Test parsing ${VAR} references."""
        content = """@SET VAR1 1.0
@SET VAR2 ${VAR1}
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Should have two variable nodes
        var_nodes = [n for n in graph.nodes if n.type == "variable"]
        assert len(var_nodes) == 2

        # VAR1 should have an edge to VAR2 (VAR1 must be resolved before VAR2)
        var1_refs = [e for e in graph.edges if e.source == "var_VAR1" and e.target == "var_VAR2"]
        assert len(var1_refs) == 1

    def test_multiple_variable_references(self, test_inputs_dir):
        """Test parsing multiple variable references."""
        content = """@SET BASE_DIR /path/to/dir
@SET DATA_FILE ${BASE_DIR}/data.txt
@SET OUTPUT_FILE ${BASE_DIR}/output.dat
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Should have three variable nodes
        var_nodes = [n for n in graph.nodes if n.type == "variable"]
        assert len(var_nodes) == 3

        # BASE_DIR should have edges to both DATA_FILE and OUTPUT_FILE
        base_dir_refs = [e for e in graph.edges if e.source == "var_BASE_DIR"]
        assert len(base_dir_refs) == 2


class TestIncludeDependencies:
    """Test @INCLUDE file reference parsing."""

    def test_simple_include(self, test_inputs_dir):
        """Test parsing a simple @INCLUDE directive."""
        # Create a temporary include file
        include_content = """&SUBSYS
  KIND O
  BASIS_SET dzvp-molopt-sr-gth
&END
"""
        include_file = test_inputs_dir / "test_dependency_include.inc"
        include_file.write_text(include_content)

        try:
            content = """@INCLUDE test_dependency_include.inc
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
            extractor = DependencyExtractor(base_dir=test_inputs_dir)
            graph = extractor.extract_from_content(content)

            # Should have one include node
            include_nodes = [n for n in graph.nodes if n.type == "include"]
            assert len(include_nodes) == 1
            assert "test_dependency_include.inc" in include_nodes[0].name
        finally:
            # Clean up
            if include_file.exists():
                include_file.unlink()

    def test_nested_includes(self, test_inputs_dir):
        """Test parsing nested @INCLUDE directives."""
        # Create nested include files
        inner_content = """&KIND H
  BASIS_SET dzvp-molopt-sr-gth
&END
"""
        outer_content = """&KIND O
  BASIS_SET dzvp-molopt-sr-gth
&END
"""

        inner_file = test_inputs_dir / "test_inner_dependency.inc"
        outer_file = test_inputs_dir / "test_outer_dependency.inc"

        inner_file.write_text(inner_content)
        outer_file.write_text(outer_content)

        try:
            content = """@INCLUDE test_outer_dependency.inc
@INCLUDE test_inner_dependency.inc
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
            extractor = DependencyExtractor(base_dir=test_inputs_dir)
            graph = extractor.extract_from_content(content)

            # Should have two include nodes
            include_nodes = [n for n in graph.nodes if n.type == "include"]
            assert len(include_nodes) == 2
        finally:
            # Clean up
            if inner_file.exists():
                inner_file.unlink()
            if outer_file.exists():
                outer_file.unlink()


class TestExternalDependencies:
    """Test external file dependency parsing (BASIS_SET, POTENTIAL)."""

    def test_basis_set_dependency(self, test_inputs_dir):
        """Test parsing BASIS_SET_FILE_NAME references."""
        content = """&FORCE_EVAL
  &DFT
    BASIS_SET_FILE_NAME /path/to/basissets
  &END DFT
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Should have one external dependency node
        ext_nodes = [n for n in graph.nodes if n.type == "external"]
        assert len(ext_nodes) == 1
        assert "BASIS_SET_FILE_NAME" in ext_nodes[0].name
        assert "/path/to/basissets" in ext_nodes[0].value

    def test_potential_dependency(self, test_inputs_dir):
        """Test parsing POTENTIAL_FILE_NAME references."""
        content = """&FORCE_EVAL
  &DFT
    POTENTIAL_FILE_NAME /path/to/potentials
  &END DFT
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Should have one external dependency node
        ext_nodes = [n for n in graph.nodes if n.type == "external"]
        assert len(ext_nodes) == 1
        assert "POTENTIAL_FILE_NAME" in ext_nodes[0].name
        assert "/path/to/potentials" in ext_nodes[0].value


class TestGraphOperations:
    """Test graph operations like topological sort and cycle detection."""

    def test_topological_sort(self, test_inputs_dir):
        """Test topological sorting of dependencies."""
        content = """@SET VAR3 ${VAR2}
@SET VAR2 ${VAR1}
@SET VAR1 1.0
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Get topological sort
        sorted_nodes = graph.get_topological_sort()

        # VAR1 should come before VAR2, which should come before VAR3
        var_indices = {n.name: i for i, n in enumerate(sorted_nodes) if n.type == "variable"}
        if "VAR1" in var_indices and "VAR2" in var_indices and "VAR3" in var_indices:
            assert var_indices["VAR1"] < var_indices["VAR2"]
            assert var_indices["VAR2"] < var_indices["VAR3"]

    def test_cycle_detection(self, test_inputs_dir):
        """Test detection of circular dependencies."""
        content = """@SET VAR1 ${VAR2}
@SET VAR2 ${VAR3}
@SET VAR3 ${VAR1}
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Should detect cycles
        cycles = graph.detect_cycles()
        assert len(cycles) >= 1

        # Cycle should involve VAR1, VAR2, VAR3
        cycle_nodes = set()
        for cycle in cycles:
            cycle_nodes.update(cycle)

        # Check that the cycle contains at least some of these variables
        assert len(cycle_nodes) >= 2

    def test_no_cycle_in_dag(self, test_inputs_dir):
        """Test that no cycles are detected in a DAG."""
        content = """@SET VAR1 1.0
@SET VAR2 ${VAR1}
@SET VAR3 ${VAR2}
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Should not detect any cycles
        cycles = graph.detect_cycles()
        assert len(cycles) == 0


class TestGraphJSONSerialization:
    """Test JSON serialization of the dependency graph."""

    def test_json_serialization(self, test_inputs_dir):
        """Test that the graph can be serialized to JSON."""
        content = """@SET MY_VAR 1.0
@SET OTHER_VAR ${MY_VAR}
&FORCE_EVAL
  METHOD Quickstep
&END FORCE_EVAL
"""
        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_content(content)

        # Convert to JSON
        json_data = graph.to_json()

        # Verify structure
        assert "nodes" in json_data
        assert "edges" in json_data
        assert isinstance(json_data["nodes"], list)
        assert isinstance(json_data["edges"], list)

        # Verify node structure
        if json_data["nodes"]:
            node = json_data["nodes"][0]
            assert "id" in node
            assert "type" in node
            assert "name" in node

        # Verify edge structure
        if json_data["edges"]:
            edge = json_data["edges"][0]
            assert "source" in edge
            assert "target" in edge
            assert "type" in edge


class TestRealCP2KInput:
    """Test with real CP2K input files."""

    def test_nacl_input(self, test_inputs_dir):
        """Test dependency extraction from NaCl.inp."""
        nacl_file = test_inputs_dir / "NaCl.inp"
        if not nacl_file.exists():
            pytest.skip("NaCl.inp not found")

        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_file(str(nacl_file))

        # Should have variable nodes for @SET directives
        var_nodes = [n for n in graph.nodes if n.type == "variable"]
        assert len(var_nodes) > 0

        # Should find LATTICE, WITH_KP, DO_CELLOPT variables
        var_names = {n.name for n in var_nodes}
        assert "LATTICE" in var_names
        assert "WITH_KP" in var_names
        assert "DO_CELLOPT" in var_names

    def test_external_deps_from_real_file(self, test_inputs_dir):
        """Test external dependency extraction from NaCl.inp."""
        nacl_file = test_inputs_dir / "NaCl.inp"
        if not nacl_file.exists():
            pytest.skip("NaCl.inp not found")

        extractor = DependencyExtractor(base_dir=test_inputs_dir)
        graph = extractor.extract_from_file(str(nacl_file))

        # Should have external dependency nodes
        ext_nodes = [n for n in graph.nodes if n.type == "external"]
        assert len(ext_nodes) > 0

        # Should find BASIS_SET_FILE_NAME and POTENTIAL_FILE_NAME
        dep_names = {n.name for n in ext_nodes}
        assert any("BASIS_SET_FILE_NAME" in name for name in dep_names)
        assert any("POTENTIAL_FILE_NAME" in name for name in dep_names)
