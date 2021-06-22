import contextlib
import pathlib
import sys

import click


@contextlib.contextmanager
def smart_open(filename=None, mode="r"):
    """A context manager to automatically read from stdin, based on https://stackoverflow.com/a/17603000/1400465"""

    assert mode in ("r", "w", "x"), "Only r and w/x modes are supported"

    filebased = filename and str(filename) != "-"

    if filebased and mode == "r":
        fhandle = open(filename, "r")
    elif filebased and mode in ("x", "w"):
        fhandle = open(filename, mode)
    elif mode == "r":
        fhandle = sys.stdin
    else:
        fhandle = sys.stdout

    try:
        yield fhandle
    finally:
        if filebased:
            fhandle.close()


def click_validate_kv(ctx, param, args):
    if isinstance(args, dict):
        return args

    parsed = {}

    for arg in args:
        try:
            parsed.update((arg.split("=", maxsplit=1),))
        except ValueError:
            raise click.BadParameter(f"format must be 'key=value', got '{arg}'") from None

    return parsed


def fhandle_argument(func):
    return click.argument("fhandle", metavar="[<file>]", type=click.File(), default="-")(func)


def yaml_option(func):
    return click.option("--yaml", "-y", is_flag=True, help="generate YAML output instead of the default JSON")(func)


def canonical_option(func):
    return click.option("-c", "--canonical", is_flag=True, help="use the canonical output format instead of the simplified one")(
        func
    )


def base_dir_option(func):
    return click.option(
        "-b",
        "--base-dir",
        type=click.Path(exists=True, file_okay=False, path_type=pathlib.Path),
        default=".",
        help="search path used for relative @include's",
        show_default=True,
    )(func)


def var_values_option(func):
    return click.option(
        "var_values",
        "-E",
        "--set",
        metavar="key=value",
        multiple=True,
        type=click.UNPROCESSED,
        callback=click_validate_kv,
        help="preset the value for a CP2K preprocessor variable",
    )(func)
