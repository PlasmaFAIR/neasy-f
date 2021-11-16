#!/usr/bin/env python3
import argparse

TEMPLATE_FILES = {
    "neasyf_type_rank": "neasy_f.type.in.f90",
    "polymorphic_get_var_rank": "neasy_f.get_var.in.f90",
    "polymorphic_put_var_rank": "neasy_f.put_var.in.f90",
    "neasyf_read_rank": "neasy_f.read.in.f90",
    "neasyf_write_rank": "neasy_f.write.in.f90",
}

MAIN_FILE = "neasy_f.in.f90"


def array(n):
    """Return array declaration for rank n"""
    return ", ".join([":"] * n)


def slice(n):
    """Return slice for rank n array"""
    return ", ".join(["1"] * n)


def make_template_dict(n):
    return {
        "n": n,
        "array(n)": array(n),
        "slice(n)": slice(n),
    }


def make_function_template(template_name, template_file, max_n):
    with open(template_file, "r") as f:
        template = f.read()

    return {
        f"mod_proc_{template_name}": "\n".join(
            f"    module procedure {template_name}{n}" for n in range(1, max_n + 1)
        ),
        template_name: "\n".join(
            template.format(**make_template_dict(n)) for n in range(1, max_n + 1)
        ),
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        "neasfy_generate_sources",
        description="Generate the main module source file for neasy-f",
    )
    parser.add_argument(
        "-n",
        "--max-rank",
        type=int,
        help="Maximum rank to generate implementations for. Note that although Fortran supports 15 dimensions, netCDF only supports 7",
        default=7,
    )
    parser.add_argument(
        "-w",
        "--write-to",
        type=str,
        help="Name of file to write output to. Prints to screen by default",
        default=None,
    )

    args = parser.parse_args()

    templates = {}
    for name, file in TEMPLATE_FILES.items():
        templates.update(make_function_template(name, file, args.max_rank))

    with open("neasy_f.in.f90", "r") as f:
        contents = f.read()

    if args.write_to is None:
        print(contents.format(**templates))
    else:
        with open(args.write_to, "w") as f:
            f.write(contents.format(**templates))
