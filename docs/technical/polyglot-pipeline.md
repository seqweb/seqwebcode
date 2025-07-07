# Polyglot Box-Passing Processing Pipeline Architecture

## 🧭 Overview

This architecture supports **polyglot pipelines** composed of modular processing **nodes**, where each node is implemented in an arbitrary language (e.g., Python, Java, Lisp, Bash). These nodes communicate via a shared abstract structure called a **box** — a language-agnostic key-value map that flows through the pipeline.

Each node is designed as a **plug-and-play module**, capable of being composed, replaced, tested, or reused independently, regardless of implementation language.

Nodes may be composed via **native orchestration** (when implemented in the same language) or executed in isolation through **IO wrappers** that expose a standardized shell or CLI interface. This hybrid model allows clean decoupling, testability, and flexibility across execution boundaries.

---

## 📦 Key Terminology

| Term                | Meaning |
|---------------------|---------|
| **box**             | A dictionary-like structure of key-value pairs shared between modules. It serves both as the input/output of each module and as a shared **common carrier** of state across pipeline stages. |
| **inbox**           | The input `box` to a node. |
| **outbox**          | The output `box` from a node, typically the inbox plus zero or more updates. |
| **node**            | A processing unit that takes an inbox and returns an outbox. |
| **module**          | A group of related node definitions implemented in a single language. |
| **program**         | A shell-invocable wrapper around a node/module exposing the inbox/outbox interface over CLI+JSON. |
| **IO wrapper**      | A thin entry point that translates CLI input to a native-language `box`, invokes the inner function, and emits JSON output. |
| **inner function**  | A native-language, pure function that maps a `box` (dict/map) to another `box`. |
| **destructuring interface** | A function signature that binds named keys from a box to function arguments (e.g., `*, prompt, noisy=False, **_rest`). |
| **box-then-kwargs pattern** | Pattern where a function accepts the full `box` and also unpacks it for destructuring. |
| **native composition** | Bypassing the IO wrapper by directly calling inner functions within the same runtime environment. |
| **shell execution** | Cross-language or system-level execution where modules run as subprocesses using wrapper interfaces. |

---

## 🔧 Implementation Patterns

### ✅ Destructuring Inner Function (Python)

```python
def normalize_prompt(box: dict, *, prompt, noisy=False, **_rest) -> dict:
    result = prompt.strip().lower()
    if noisy:
        print(f"\n\t🧹 Normalized Prompt:\n{result}")
    return {**box, "normalized_prompt": result}
```

- Uses both full box and destructuring
- Extra keys (e.g. `config`) are preserved even if unused
- Enables composability and pass-through semantics

---

### ✅ CLI Wrapper (Python)

```python
def main():
    parser = argparse.ArgumentParser(description="Run a KA pipeline stage.")
    parser.add_argument("prompt")
    parser.add_argument("--noisy", action="store_true")
    parser.add_argument("--config")

    args = parser.parse_args()
    inbox = {"prompt": args.prompt, "noisy": args.noisy, "config": args.config}
    outbox = normalize_prompt(inbox, **inbox)
    json.dump(outbox, sys.stdout)
```

---

## 🔁 Orchestration Example: Native Python

```python
from typing import Callable, Dict, List, Optional

def run_pipeline(
    stages: Optional[List[Callable]] = None, initial_box: Optional[Dict] = None
) -> Dict:
    """Run a sequence of stages on a box, using the box-then-kwargs pattern."""
    if stages is None:
        stages = []
    if initial_box is None:
        initial_box = {}

    box = initial_box
    for stage in stages:
        box = stage(box, **box)
    return box
```

- Orchestrates native Python stages
- Supports implicit pass-through of `config`, `noisy`, etc.
- Easily extended with conditional logic, tracing, or logging

---

## 🌐 Cross-Language Execution Model

When nodes are implemented in different languages and invoked as standalone programs:

- Each **program derives its `inbox` from its command-line arguments**, using the language's CLI parsing conventions (e.g., `argparse` in Python, `getopts` in Bash, etc.).
- These arguments effectively form a **shell-flavored keyword map**, which becomes the initial `inbox`.
- The program then passes this inbox to the corresponding **inner function** (using the destructuring pattern if supported).
- The result is a new **outbox**, which is serialized to **JSON** and written to stdout (or another standard stream or file as appropriate).
- **Optionally**, a program may support a special flag (e.g. `--inbox-json`) to preload an initial inbox from a JSON object, merging it with the CLI-derived args — but this is not required.
- If no such preload mechanism is used, the program should default to starting from an **empty box** and building it solely from the command-line arguments.

This design allows for:
- Simple shell usage (`myprog --foo bar --noisy`)
- Compatibility with language-native command-line tooling
- Optional integration with more structured JSON-based pipelines when needed

### 🔁 Program Reusability

By structuring programs to build their inbox from command-line arguments (optionally merging with a JSON seed), each module becomes naturally reusable in multiple contexts:

- ✅ **As a stage** in a larger **polyglot pipeline**, invoked by an orchestrator.
- ✅ **As a standalone CLI tool**, usable directly by developers or scripts.
- ✅ **In isolation for testing**, with inboxes constructed in code or passed via CLI.

This versatility makes it easy to scale from interactive experimentation to production pipelines without changing the module’s core logic.

---

## 📘 Architectural Principles

1. **Destructurable box interface** — Functions only bind what they need.
2. **Non-destructive box mutation** — Modules are encouraged to preserve and extend the box, not replace it.
3. **Interop via wrappers, speed via direct calls** — Same-language modules can call each other directly, avoiding shell overhead.
4. **Soft schema contracts** — Boxes have loose structure; nodes declare required keys but tolerate unknown ones.
5. **Composable orchestration** — Pipelines can be expressed as ordered lists of stage functions or shell commands.
6. **Complete box input** — All parameters required for a module’s operation must be present in the input box, either directly or by reference. This supports functional purity, reproducibility, and testability.
7. **Box as common carrier** — The box persists across pipeline stages and may accumulate context not just for immediate use but for downstream consumers, metadata propagation, or audit trail enrichment.





