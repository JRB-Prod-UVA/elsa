#!/bin/env python3
import os
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
NUM_RUNS = 10
OUTPUT_FILENAME = "performance_report.tex"
GRAPH_FILENAME = "performance_graph.pgf"

# Configure matplotlib for LaTeX output
rcParams.update(
    {
        "pgf.texsystem": "pdflatex",
        "font.family": "serif",
        "text.usetex": True,
        "pgf.rcfonts": False,
    }
)


def measure_program(program, args=None, num_runs=5):
    """Measure execution time of a program over multiple runs."""
    times = []
    avg_times = []
    std_devs = []

    # Every test file
    for arg in args:
        # Number of times to measure
        for _ in range(num_runs):
            start_time = time.perf_counter()
            subprocess.run(
                [program, arg],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            end_time = time.perf_counter()
            times.append(end_time - start_time)

        avg_time = statistics.mean(times)
        avg_times.append(avg_time)
        std_dev = statistics.stdev(times) if len(times) > 1 else 0
        std_devs.append(std_dev)
        # reset times
        times = []

    return avg_times, std_devs


def generate_latex_document(avg_time, std_dev, graph_filename):
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

The program \texttt{%s} was executed %d times. The average execution time and standard deviation are shown below.

\begin{table}[h]
\centering
\begin{tabular}{cc}
\toprule
Average Time (s) & Standard Deviation (s) \\
\midrule
%.4f & %.4f \\
\bottomrule
\end{tabular}
\caption{Execution Time Statistics}
\end{table}

\begin{figure}[h]
\centering
\input{%s}
\caption{Program execution time with error bars representing one standard deviation.}
\end{figure}

\end{document}
""" % (
        PROGRAM_TO_TEST,
        NUM_RUNS,
        avg_time,
        std_dev,
        graph_filename,
    )

    return latex_template


def create_bar_plot(avg_times, std_devs, filename, input_files):
    """Create a bar plot with error bars and save as pgf."""
    fig, ax = plt.subplots(figsize=(6, 4), layout="constrained")
    items = zip(avg_times, std_devs, input_files)

    for avg_time, std_dev, input_file in items:
        bar = ax.bar(
            [input_file], [avg_time], yerr=[std_dev], capsize=10, color="skyblue"
        )
        # Annotate the exact values
        ax.bar_label(bar, labels=[f"{avg_time:.4f} ± {std_dev:.4f} s"], padding=3)

    ax.set_ylabel("Time (seconds)")
    ax.set_title(f"Average Execution Time of {PROGRAM_TO_TEST}")

    plt.savefig(filename)


def main():
    args = ["test.lc", "test2.lc"]
    # Measure the program's performance
    avg_times, std_devs = measure_program(PROGRAM_TO_TEST, num_runs=NUM_RUNS, args=args)

    print(f"Average time: {avg_times[-1]:.4f} s")
    print(f"Standard deviation: {std_devs[-1]:.4f} s")

    # Create the graph
    create_bar_plot(avg_times, std_devs, GRAPH_FILENAME, args)

    # Generate LaTeX document
    latex_doc = generate_latex_document(avg_times[-1], std_devs[-1], GRAPH_FILENAME)

    with open(OUTPUT_FILENAME, "w") as f:
        f.write(latex_doc)

    print(f"LaTeX report saved to {OUTPUT_FILENAME}")


if __name__ == "__main__":
    main()
