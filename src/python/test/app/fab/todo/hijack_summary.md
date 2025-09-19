# Hijacked Main Functions Summary

## Complete List of Potentially Hijacked Source Files in `app/fab/`

| # | Source File | Test File Path | Hijacking Level | Notes |
|---|-------------|----------------|-----------------|-------|
| 1 | `standardize_id.py` | `test/app/fab/test_standardize_id.py` | **LOW** | Clean wrapper, minimal testing |
| 2 | `print_graph.py` | `test/app/fab/test_print_graph.py` | **HIGH** | Creates test graph, extensive testing logic |
| 3 | `dump_graph.py` | `test/app/fab/test_dump_graph.py` | **HIGH** | Creates test graph, testing-specific JSON output |
| 4 | `make_one.py` | `test/app/fab/test_make_one.py` | **MED** | Testing-specific JSON output, but minimal test setup |
| 5 | `get_metadata.py` | `test/app/fab/test_get_metadata.py` | **HIGH** | Creates test graph, extensive testing setup |
| 6 | `add_raw_text.py` | `test/app/fab/test_add_raw_text.py` | **HIGH** | Creates test graph and data, testing-specific logic |
| 7 | `init_sequence.py` | `test/app/fab/test_init_sequence.py` | **HIGH** | Creates test graph, testing-specific setup |
| 8 | `init_graph.py` | `test/app/fab/test_init_graph.py` | **LOW** | Clean wrapper following polyglot pattern |
| 9 | `get_oeis_data.py` | `test/app/fab/test_get_oeis_data.py` | **LOW** | Clean wrapper following polyglot pattern |
| 10 | `segment_sections.py` | `test/app/fab/test_segment_sections.py` | **LOW** | Clean wrapper, testing-focused but proper |
| 11 | `process_sections.py` | `test/app/fab/test_process_sections.py` | **LOW** | Clean wrapper, testing-focused but proper |
| 12 | `add_sections.py` | `test/app/fab/test_add_sections.py` | **LOW** | Clean wrapper, testing-focused but proper |
| 13 | `make_range.py` | `test/app/fab/test_make_range.py` | **LOW** | Clean fabricator wrapper |
| 14 | `make_list.py` | `test/app/fab/test_make_list.py` | **LOW** | Clean fabricator wrapper |
| 15 | `summarize_metadata.py` | `test/app/fab/test_summarize_metadata.py` | **LOW** | Clean utility wrapper |
| 16 | `summarize_list.py` | `test/app/fab/test_summarize_list.py` | **LOW** | Clean utility wrapper |
| 17 | `test_fab.py` | `test/app/fab/test_test_fab.py` | **LOW** | Clean fabricator wrapper (already a test) |
| 18 | `mod0.py` | `test/app/fab/test_mod0.py` | **LOW** | Clean wrapper following polyglot pattern |
| 19 | `fab0.py` | `test/app/fab/test_fab0.py` | **LOW** | Clean fabricator wrapper |
| 20 | `mod2.py` | `test/app/fab/test_mod2.py` | **LOW** | Clean wrapper following polyglot pattern |

## Summary

- **HIGH hijacking (5 files)**: Need significant extraction of testing logic
  - `print_graph.py`, `dump_graph.py`, `get_metadata.py`, `add_raw_text.py`, `init_sequence.py`
- **MED hijacking (1 file)**: Need moderate extraction of testing-specific output
  - `make_one.py`
- **LOW hijacking (14 files)**: Clean wrappers, minimal extraction needed

## Priority for Draft Test Files

The **HIGH** and **MED** files are the priority for creating draft test files with "TODO" comments, as they contain the most testing logic that needs to be extracted from their `main` functions.
