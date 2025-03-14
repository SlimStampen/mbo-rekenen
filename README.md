# MBO rekenen

As part of the MBOin2030-funded project, students at Noorderpoort and Alfa-college practiced fundamental mathematics knowledge with MemoryLab.
This repository contains an analysis of the practice data, the pretest and posttest, as well as the feedback from students.

## Analysis notebooks

- [Practice activity](output/01_practice_activity.md)
- [Test performance](output/02_test_performance.md)
- [Evaluation](output/03_evaluation.md)

## Usage

Practice activity, test performance, and evaluation are all generated from R Markdown files.
Note that database access is required for analysis of the practice data, and that test data are not included in this repository to preserve anonymity.

Recreate each analysis individually by running one of the following commands:
```bash
make activity
make performance
make evaluation
```

Or recreate all at once:
```bash
make all
```