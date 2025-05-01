# Autocomplete and Verify

**Autocomplete and Verify** is an Emacs package that combines AI-powered code generation with formal static analysis using [Meta's Infer](https://fbinfer.com/). This tool allows you to:

-  Autocomplete code snippets using natural language prompts.
-  Automatically analyze the generated code for bugs.
-  Explain formal pre- and postcondition violations in plain English.
-  Optionally apply suggested fixes using ChatGPT.
-  Highlight problematic lines and generate explanations in side-by-side buffers.

---

##  Features

- Context-aware code generation using ChatGPT (OpenAI API).
- Formal verification using Meta's Infer (`--pulse-only`).
- Natural language explanations of static analysis issues.
- Automatic installation of Infer.
- Optional iterative auto-fixing using LLM.
- Inline code highlighting for problematic lines.
- Side-by-side window display of code and analysis.

---

## Installation

1. Clone this repo (not on MELPA yet!):

    ```bash
    git clone https://github.com/RaghavRoy145/autocomplete-and-verify ~/.emacs.d/lisp/autocomplete-and-verify
    ```

2. Add to your Emacs `init.el`:

    ```elisp
    ;; Load Autocomplete and Verify
    (load-file "~/.emacs.d/lisp/autocomplete-and-verify/autocomplete-and-verify.el")
    (require 'autocomplete-and-verify)

    ;; Optional: bind to a key (e.g., C-c v)
    (add-hook 'prog-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c v") 'autocomplete-and-verify-run)))
    ```

3. Restart Emacs.

---

## Usage

### 1. Add a comment prompt

Write a comment in your C buffer like this:


// @autocomplete: Write a function that adds two numbers and prints the result

### 2. Run the command

Press the keybinding (default C-c v) or run:

```M-x autocomplete-and-verify-run```

This will

- Generate code using ChatGPT based on your comment.

- Run Meta’s Infer static analysis on the generated code.

- Highlight error lines and explain them in another buffer.

3. (Optional) Fix issues

## ChatGPT API Setup

This package uses the OpenAI API for code generation and bug explanation.

When you first run the package, it will prompt you for your API key.

You can also hardcode your API key in your config:

```(setq copilot-infer-api-key "sk-...")```

## Screen Grab of The Pipeline 

### _LLM Code Gen -> Infer Finds Bugs and Prompts LLM With Error -> LLM Explains Bug and Proposes Fix -> Fixed Code Runs_



https://github.com/user-attachments/assets/7ee48d8c-3f4c-41de-9846-7253ef35960b



