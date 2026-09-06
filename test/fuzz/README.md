# Fuzz

These scripts look for bad error messages by taking a program that works and
breaking one spot in it, then counting what the compiler printed, because one
typo should give you one error and not twenty.

```
$ dune build
$ python3 test/fuzz/test_delim_perms.py
ran 5760 delimiter arrangements
75 FAILURES Counter({'POISON': 75})

--- POISON ctx=params sub='(){}[]'
func f(a: i32 (){}[]) i32 { return a }
func main() i32 { return 0 }

error: expected `)`
  at main.rp:1:15
    func f(a: i32 (){}[]) i32 { return a }
                  ^ found (
error: type mismatch
  at main.rp:1:15
    func f(a: i32 (){}[]) i32 { return a }
                  ^~ expected <error>, found ()
```

A script exits 0 when nothing failed and you can set `JOBS` to change how many
processes it runs on or `RIPEC` to point at a different build.

## Scripts

| Script | Args | What it does |
| --- | --- | --- |
| `test_mutations.py` | `--stride N` `-v` | Takes one working program per language feature and edits every token in it |
| `test_type_positions.py` | `-v` | Puts a broken type in every spot a type is allowed |
| `test_recovery.py` | `<seed> <runs>` | Makes up broken structs, enums, and functions |
| `test_delim_mixes.py` | | Drops, doubles, or swaps one bracket |
| `test_delim_perms.py` | | Tries every possible order of the six brackets |
| `test_delim_random.py` | `<seed> <runs>` | Messes up brackets in random spots |
| `test_cluster_failures.py` | `<top>` `--show TEXT` | Finds which bug is behind the most failures |
| `test_no_regression.py` | `<rev>` `--show TEXT` | Compares this build against an older one |
| `test_gen_goldens.py` | `<dir>` `--write` | Updates the saved output files in `test/programs` |

## Where it stands

These numbers come from `601fd7a`.

| Script | Cases | Failures | Worst |
| --- | --- | --- | --- |
| `test_mutations.py` | 50831 | 120 | 10 |
| `test_cluster_failures.py` | 50831 | 120 in 71 groups | |
| `test_delim_perms.py` | 5760 | 75 | |
| `test_recovery.py` | 1200 | 4 | 5 |
| `test_type_positions.py` | 1062 | 0 | 2 |
| `test_delim_random.py` | 400 | 0 | |
| `test_delim_mixes.py` | 150 | 0 | |

## Contexts

The programs live in `contexts/<script>/` as `.rp.in` files and each one is a
single construct with `%%D%%` where the script writes. The `corpus`
directory is a bit different because those are working programs, one per
language feature.
