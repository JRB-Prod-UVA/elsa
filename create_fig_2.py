#!/bin/env python3
import os
import re
import statistics
import subprocess
import time

import matplotlib
import matplotlib.pyplot as plt
from matplotlib import rcParams

matplotlib.use("pgf")  # Set backend to pgf before importing pyplot

# Change directory to current script directory
abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

# Configuration
PROGRAM_TO_TEST = "elsa"
NUM_RUNS = 5
OUTPUT_FILENAME = "performance_report_2.tex"
GRAPH_FILENAME = "performance_graph_2.pgf"
HASKELL_SOURCE = "src/Language/Elsa/Eval.hs"
INPUT_FILE = "test2.lc"
WEIGHTS = [round(i * 0.1, 1) for i in range(11)]  # 0.0 to 1.0 in 0.1 steps

# Configure matplotlib for LaTeX output
rcParams.update(
    {
        "pgf.texsystem": "pdflatex",
        "font.family": "serif",
        "text.usetex": True,
        "pgf.rcfonts": False,
    }
)


def modify_haskell_weight(new_weight):
    """Update the appCountWeight in Haskell source file using regex."""
    with open(HASKELL_SOURCE, "r") as f:
        content = f.read()

    # Replace the weight value
    new_content = re.sub(
        r"appCountWeight :: Float\nappCountWeight = \d+\.\d+",
        f"appCountWeight :: Float\nappCountWeight = {new_weight}",
        content,
    )

    with open(HASKELL_SOURCE, "w") as f:
        f.write(new_content)

    # Recompile the program
    compile_result = subprocess.run(
        ["stack", "install"], stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )

    if compile_result.returncode != 0:
        raise RuntimeError(
            f"Compilation failed with weight {new_weight}:\n"
            f"{compile_result.stderr.decode()}"
        )


def measure_program(program, input_file, num_runs=5):
    """Measure execution time of a program over multiple runs."""
    times = []

    for _ in range(num_runs):
        start_time = time.perf_counter()
        subprocess.run(
            [program, input_file],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        end_time = time.perf_counter()
        times.append(end_time - start_time)

    avg_time = statistics.mean(times)
    std_dev = statistics.stdev(times) if len(times) > 1 else 0

    return avg_time, std_dev


def generate_latex_document(graph_filename):
    """Generate a complete LaTeX document with the results."""
    latex_template = r"""
\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{graphicx}
\usepackage{pgf}
\usepackage{amsmath}
\usepackage{booktabs}

\title{Program Performance Report}
\author{Automated Measurement}
\date{\today}

\begin{document}

\maketitle

\section{Performance Results}

The program \texttt{%s} was executed with different weights (%d runs per weight) using input file \texttt{%s}.

\begin{figure}[h]
\centering
\input{%s}
\caption{Program execution time across different weights with error bars representing one standard deviation.}
\end{figure}

\end{document}
""" % (
        PROGRAM_TO_TEST,
        NUM_RUNS,
        INPUT_FILE,
        graph_filename,
    )

    return latex_template


def create_line_plot(weights, avg_times, std_devs, filename):
    """Create a line plot with error bars and save as pgf."""
    fig, ax = plt.subplots(figsize=(5, 4), layout="constrained")

    # Plot main line
    ax.plot(weights, avg_times, "o-", color="skyblue", label="Average Time")

    # Add error bars
    ax.errorbar(
        weights,
        avg_times,
        yerr=std_devs,
        fmt="none",
        ecolor="gray",
        capsize=5,
        capthick=2,
        alpha=0.7,
    )

    # Annotate points
    for w, t, s in zip(weights, avg_times, std_devs):
        ax.text(w, t + s + 0.02, f"{t:.2f}±{s:.2f}s", ha="center", fontsize=8)

    ax.set_xlabel("appCountWeight")
    ax.set_ylabel("Time (seconds)")
    ax.set_title(f"Execution Time of {PROGRAM_TO_TEST} vs. appCountWeight")
    ax.set_xticks(weights)
    ax.grid(True, linestyle="--", alpha=0.7)

    plt.savefig(filename)


def main():
    avg_times = []
    std_devs = []

    for weight in WEIGHTS:
        print(f"Testing with weight {weight:.1f}")

        # Modify Haskell source and recompile
        modify_haskell_weight(weight)

        # Measure performance
        avg_time, std_dev = measure_program(PROGRAM_TO_TEST, INPUT_FILE, NUM_RUNS)
        avg_times.append(avg_time)
        std_devs.append(std_dev)

        print(f"  Average: {avg_time:.4f}s ± {std_dev:.4f}s")

    # Create the graph
    create_line_plot(WEIGHTS, avg_times, std_devs, GRAPH_FILENAME)

    # Generate LaTeX document
    latex_doc = generate_latex_document(GRAPH_FILENAME)

    with open(OUTPUT_FILENAME, "w") as f:
        f.write(latex_doc)

    print(f"LaTeX report saved to {OUTPUT_FILENAME}")


if __name__ == "__main__":
    main()
