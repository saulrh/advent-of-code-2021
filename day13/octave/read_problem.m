function problem = read_problem(points_filename, folds_filename)
  problem.points = csvread(points_filename)(:, 2:3);

  folds_file = fopen(folds_filename, 'r');
  folds = textscan(folds_file, "%d %s %d", "Delimiter", ",");
  fclose(folds_file);

  problem.axes = cellfun(@translate_axis, folds{1, 2});
  problem.positions = folds{1, 3};
endfunction

## Local Variables:
## mode: octave
## End:
