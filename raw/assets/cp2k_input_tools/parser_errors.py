class ParserError(Exception):
    pass


class InvalidNameError(ParserError):
    pass


class SectionMismatchError(ParserError):
    pass


class InvalidSectionError(ParserError):
    pass


class InvalidParameterError(ParserError):
    pass


class NameRepetitionError(ParserError):
    pass


class PreprocessorError(ParserError):
    pass
