"""CLI entry point for tech debt agents."""

import argparse
import sys


def main():
    parser = argparse.ArgumentParser(description="Tech debt agent runner")
    sub = parser.add_subparsers(dest="command", required=True)

    auditor_cmd = sub.add_parser("auditor", help="Run the auditor agent")
    auditor_cmd.add_argument("--force", action="store_true", help="Bypass interactive session check")

    fixer_cmd = sub.add_parser("fixer", help="Run the fixer agent")
    fixer_cmd.add_argument("--force", action="store_true", help="Bypass interactive session check")

    args = parser.parse_args()

    if args.command == "auditor":
        from .auditor import main as run
        run(force=args.force)
    elif args.command == "fixer":
        from .fixer import main as run
        run(force=args.force)
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
