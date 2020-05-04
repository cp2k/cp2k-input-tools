import pytest


@pytest.fixture
def parser():
    from cp2k_input_tools.parser import CP2KInputParser
    from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML

    return CP2KInputParser(DEFAULT_CP2K_INPUT_XML)
