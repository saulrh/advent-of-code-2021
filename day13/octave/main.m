#!/usr/bin/octave -qf

example = read_problem("../example_problem1.csv", "../example_problem2.csv");
input = read_problem("../input_problem1.csv", "../input_problem2.csv");

[example_pt_counts, example_final_pts] = do_folds(example);
assert(example_pt_counts(1) == 17);

[input_pt_counts, input_final_pts] = do_folds(input);
assert(input_pt_counts(1) == 706);

render(input_final_pts)

